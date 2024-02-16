package com.github.windymelt.zmm.application

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.effect.std.Mutex
import com.github.windymelt.zmm.application.movieGenaration.{
  AudioQueryFetcher,
  DictionaryApplier,
  HtmlBuilder,
  IndicatorHelper,
  WavGenerator,
  XmlUtil
}
import com.github.windymelt.zmm.domain.model.{Context, VoiceBackendConfig}
import com.github.windymelt.zmm.{domain, infrastructure, util}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import cats.syntax.parallel._

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.language.postfixOps

class GenerateMovie(
    filePath: String,
    outPathString: String,
    logLevel: String = "INFO"
) extends domain.repository.FFmpegComponent
    with infrastructure.FFmpegComponent
    with domain.repository.VoiceVoxComponent
    with infrastructure.VoiceVoxComponent
    with domain.repository.ScreenShotComponent
    with infrastructure.ChromeScreenShotComponent
    with util.UtilComponent
    with IndicatorHelper {

  implicit def logger: Logger[IO] = Slf4jLogger.getLogger[IO]
  private def dictionaryApplier = new DictionaryApplier()
  private def audioQueryFetcher = new AudioQueryFetcher()
  private def xmlUtil = new XmlUtil()
  private def htmlBuilder = new HtmlBuilder()
  private def wavGenerator = new WavGenerator(logLevel)
  def voiceVox: VoiceVox = new ConcreteVoiceVox(voiceVoxUri)

  def ffmpeg =
    new ConcreteFFmpeg(
      config.getString("ffmpeg.command"),
      verbosity = logLevel match {
        case "DEBUG" => ConcreteFFmpeg.Verbose
        case "TRACE" => ConcreteFFmpeg.Verbose
        case _       => ConcreteFFmpeg.Quiet
      }
    ) // TODO: respect construct parameter

  val voiceVoxUri =
    sys.env.get("VOICEVOX_URI") getOrElse config.getString("voicevox.apiUri")

  val chromiumCommand =
    sys.env.get("CHROMIUM_CMD").getOrElse(config.getString("chromium.command"))

  val chromiumNoSandBox = sys.env
    .get("CHROMIUM_NOSANDBOX")
    .map(_ == "1")
    .getOrElse(config.getBoolean("chromium.nosandbox"))

  def screenShotResource: IO[Resource[IO, ScreenShot]] = {
    for {
      _ <- logger.debug(
        s"chromium command: $chromiumCommand, chromoumNoSandBox: $chromiumNoSandBox"
      )
      mu <- Mutex[IO]
    } yield mu.lock.map { _ =>
      new ChromeScreenShot(
        chromiumCommand,
        logLevel match {
          case "TRACE" => ChromeScreenShot.Verbose
          case "DEBUG" => ChromeScreenShot.Verbose
          case _       => ChromeScreenShot.Quiet
        },
        chromiumNoSandBox
      )
    }
  }

  def execute: IO[Unit] = {
    val xmlElem = xmlUtil.readXml(filePath)

    for {
      _ <- logger.debug(s"generate($filePath, $outPathString)")
      _ <- logger.debug(s"pwd: ${System.getProperty("user.dir")}")
      _ <- logger.debug(s"voicevox api: ${voiceVoxUri}")
      _ <- logger.debug(
        s"""ffmpeg command: ${config.getString("ffmpeg.command")}"""
      )
      x <- xmlElem
      _ <- xmlUtil.sanitize(x)
      defaultCtx <- prepareDefaultContext(x)
      _ <- dictionaryApplier.execute(defaultCtx.dict)
      // dialogue要素から取得したセリフとコンテキストのペアを作成する
      sayCtxPairs <- IO.pure(
        Context.fromNode((x \ "dialogue").head, defaultCtx)
      )
      // dialogue要素から取得したセリフを元に音声ファイルを作成する
      voices <- {
        val saySeq = sayCtxPairs map {
          case (s, ctx) if ctx.spokenByCharacterId.contains("silent") =>
            wavGenerator.generateSilence(ctx)
          case (s, ctx) => wavGenerator.generateSay(s, ctx)
        }
        saySeq.parSequence
      }
      // 読み上げ長をContextに追加する。母音情報が得られた場合も追加する
      sayCtxPairs <- IO.pure {
        val pairs = sayCtxPairs zip voices
        pairs map {
          case ((say, context), (_, dur, Seq())) =>
            (say, context.copy(duration = Some(dur)))
          case ((say, context), (_, dur, vowels)) =>
            (
              say,
              context.copy(spokenVowels = Some(vowels), duration = Some(dur))
            )
        }
      }
      // Contextにフィルタを適用する。母音情報を元に立ち絵を差し替えたコンテキストに分割したりする
      sayCtxPairs <- IO.pure(applyFilters(sayCtxPairs))
      // この時点でvideoとaudioとの間に依存がないので並列実行する
      // BUG: SI-5589 により、タプルにバインドできない
      va <- backgroundIndicator("Generating video and concatenated audio").use {
        _ =>
          val paths = voices.map(_._1)
          generateVideo(sayCtxPairs, paths) product ffmpeg
            .concatenateWavFiles(paths.map(_.toString))
      }
      (video, audio) = va
      zippedVideo <- backgroundIndicator("Zipping silent video and audio").use {
        _ => ffmpeg.zipVideoWithAudio(video, audio)
      }
      composedVideo <- backgroundIndicator("Composing Video").surround {
        // もし設定されていればビデオを合成する。BGMと同様、同じビデオであれば結合する。
        val videoWithDuration: Seq[(Option[os.Path], FiniteDuration)] =
          sayCtxPairs
            .map(p =>
              p._2.video.map(path =>
                os.pwd / os.RelPath(util.PathAlias.resolve(path, "ffmpeg"))
              ) -> p._2.duration.get
            )

        val reductedVideoWithDuration = groupReduction(videoWithDuration)

        // 環境によっては上書きに失敗する？ので出力ファイルが存在する場合削除する
        val outputFile = os.pwd / "output_composed.mp4"
        os.remove(outputFile, checkExists = false)

        reductedVideoWithDuration.filter(_._1.isDefined).size match {
          case 0 =>
            IO.delay {
              os.move(zippedVideo, outputFile)
              outputFile
            }
          case _ =>
            ffmpeg.composeVideoWithDuration(
              zippedVideo,
              reductedVideoWithDuration
            )
        }
      }
      _ <- backgroundIndicator("Applying BGM").use { _ =>
        // BGMを合成する。BGMはコンテキストで割り当てる。sayCtxPairsでsayごとにコンテキストが確定するので、同じBGMであれば結合しつつ最終的なDurationを計算する。
        // たとえば、BGMa 5sec BGMa 5sec BGMb 10sec であるときは、 BGMa 10sec BGMb 10secに簡約される。
        val bgmWithDuration: Seq[(Option[os.Path], FiniteDuration)] =
          sayCtxPairs
            .map(p =>
              p._2.bgm.map(path =>
                os.pwd / os.RelPath(util.PathAlias.resolve(path, "ffmpeg"))
              ) -> p._2.duration.get
            )

        val reductedBgmWithDuration = groupReduction(bgmWithDuration)

        // 環境によっては上書きに失敗する？ので出力ファイルが存在する場合削除する
        val outputFilePath = os.Path(outPathString)
        os.remove(outputFilePath, checkExists = false)

        reductedBgmWithDuration.filter(_._1.isDefined).size match {
          case 0 =>
            IO.pure(
              os.move(composedVideo, outputFilePath)
            ) // Dirty fix. TODO: fix here
          case _ =>
            ffmpeg.zipVideoWithAudioWithDuration(
              composedVideo,
              reductedBgmWithDuration,
              outputFilePath
            )
        }
      }
      _ <- logger.info(s"Done! Generated to $outPathString")
    } yield ()

  }

  private def prepareDefaultContext(elem: scala.xml.Elem): IO[Context] = {
    val voiceConfigMap = xmlUtil.extractVoiceConfigMap(elem)
    val characterConfigMap = xmlUtil.extractCharacterConfigMap(elem)
    val defaultBackgroundImage = xmlUtil.extractDefaultBackgroundImage(elem)
    val defaultFont = xmlUtil.extractDefaultFont(elem)
    // 発音調整などに使う文字列辞書。今のところVOICEVOXの発音辞書に使っている
    // (word, pronounce, accent lower point)
    val dict = xmlUtil.extractPronounceDict(elem)
    val codes: Map[String, (String, Option[String])] =
      xmlUtil.extractCodes(elem)
    val maths = xmlUtil.extractMaths(elem)

    IO.pure(
      domain.model.Context(
        voiceConfigMap,
        characterConfigMap,
        defaultBackgroundImage,
        dict = dict,
        codes = codes,
        maths = maths,
        font = defaultFont
      )
    )
  }

  private def applyFilters(
      pairs: Seq[(domain.model.Say, Context)]
  ): Seq[(domain.model.Say, Context)] = {
    // フィルタが増えたら合成して伸ばす
    val composedFilters = domain.model.Filter.talkingMouthFilter
    // Arrow.secondを使うとタプルの右側だけflatMapし、左側を補完させることができる
    pairs.flatMap(composedFilters.second.run)
  }

  private def generateVideo(
      sayCtxPairs: Seq[(domain.model.Say, Context)],
      paths: Seq[fs2.io.file.Path]
  ): IO[os.Path] = {
    import cats.syntax.parallel._

    val fileCheck: String => IO[Boolean] = p =>
      IO(os.exists(os.pwd / os.RelPath(p)))

    // スクリーンショットは重いのでHTMLの内容をもとにキャッシュする(HTMLが同一内容なら同一のスクリーンショットになるという前提)
    val shot: ScreenShot => (domain.model.Say, Context) => IO[os.Path] =
      (ss: ScreenShot) =>
        (s: domain.model.Say, ctx: Context) => {
          val htmlIO = buildHtmlFile(s.text, ctx)
          for {
            stream <- htmlIO.map(s =>
              fs2.Stream[IO, Byte](s.getBytes().toSeq: _*)
            )
            html <- htmlIO
            sha1Hex <- sha1HexCode(html.getBytes())
            htmlPath = s"./artifacts/html/${sha1Hex}.html"
            htmlFile <- fileCheck(htmlPath).ifM(
              IO.pure(fs2.io.file.Path(htmlPath)),
              writeStreamToFile(stream, htmlPath)
            )
            _ <- fileCheck(s"${htmlPath}.png").ifM(
              logger.debug(s"Cache HIT: ${htmlPath}.png"),
              logger.debug(s"Cache expired: ${htmlPath}.png")
            )
            screenShotFile <- fileCheck(s"${htmlPath}.png").ifM(
              IO.pure(
                os.pwd / os.RelPath(s"${htmlPath}.png")
              ),
              ss.takeScreenShot(
                os.pwd / os.RelPath(htmlFile.toString)
              )
            )
          } yield screenShotFile
        }

    for {
      ss <- screenShotResource
      imgs <- for {
        sceneImages <- sayCtxPairs.map { pair =>
          ss.use { ss => shot(ss).tupled(pair) }
        }.parSequence
        concatenatedImages <- ffmpeg.concatenateImagesWithDuration(
          sceneImages.zip(sayCtxPairs.map(_._2.duration.get))
        )
      } yield concatenatedImages

    } yield imgs
  }

  private def buildHtmlFile(serif: String, ctx: Context): IO[String] = {
    htmlBuilder.build(serif, ctx, debuggingInfo(ctx))
  }

  private def debuggingInfo(ctx: Context): Seq[String] = {
    Seq(
      s"chromiumCommand: ${chromiumCommand}",
      s"chromiumNoSandBox: ${chromiumNoSandBox}",
      s"ctx.spokenVowels: ${ctx.spokenVowels}",
      s"ctx.currentVowel: ${ctx.currentVowel}",
      s"ctx.tachieUrl: ${ctx.tachieUrl}",
    )
  }
}

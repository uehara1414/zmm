package com.github.windymelt.zmm.application

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.effect.std.Mutex
import com.github.windymelt.zmm.application.movieGenaration.{AudioQueryFetcher, DictionaryApplier, HtmlBuilder, WavGenerator, XmlUtil}
import com.github.windymelt.zmm.domain.model.{Context, VoiceBackendConfig}
import com.github.windymelt.zmm.{domain, infrastructure, util}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

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
    with util.UtilComponent {

  implicit def logger: Logger[IO] = Slf4jLogger.getLogger[IO]
  private def dictionaryApplier = new DictionaryApplier()
  private def audioQueryFetcher = new AudioQueryFetcher()
  private def xmlSanitizer = new XmlUtil()
  private def htmlBuilder = new HtmlBuilder()
  private def wavGenerator = new WavGenerator()
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

  val voiceVoxUri = sys.env.get("VOICEVOX_URI") getOrElse config.getString("voicevox.apiUri")

  def execute: IO[Unit] = {
    val content = IO.delay(scala.xml.XML.loadFile(filePath))

    for {
      _ <- logger.debug(s"generate($filePath, $outPathString)")
      _ <- logger.debug(s"pwd: ${System.getProperty("user.dir")}")
      _ <- logger.debug(s"voicevox api: ${voiceVoxUri}")
      _ <- logger.debug(
        s"""ffmpeg command: ${config.getString("ffmpeg.command")}"""
      )
      x <- content
      _ <- xmlSanitizer.sanitize(x)
      defaultCtx <- prepareDefaultContext(x)
      _ <- dictionaryApplier.execute(defaultCtx.dict)
      sayCtxPairs <- IO.pure(
        Context.fromNode((x \ "dialogue").head, defaultCtx)
      )
      voices <- {
        import cats.syntax.parallel._
        val saySeq = sayCtxPairs map {
          case (s, ctx)
              if ctx.spokenByCharacterId == Some(
                "silent"
              ) => // TODO: voiceconfigまで辿る
            generateSilence(ctx)
          case (s, ctx) =>
            generateSay(s, voiceVox, ctx)
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
      // Contextにフィルタを適用する
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

  private def withColor(color: String) = (s: String) =>
    s"${color.toString()}${s}${scala.io.AnsiColor.RESET}"

  private def prepareDefaultContext(elem: scala.xml.Elem): IO[Context] = {
    val voiceConfigList = elem \ "meta" \ "voiceconfig"
    val voiceConfigMap: Map[String, VoiceBackendConfig] = voiceConfigList.map {
      vc =>
        vc \@ "backend" match {
          case "voicevox" =>
            val vvc = vc \ "voicevoxconfig"
            val voiceVoxSpeakerId = vvc \@ "id"
            (vc \@ "id", domain.model.VoiceVoxBackendConfig(voiceVoxSpeakerId))
          case "silent" =>
            (vc \@ "id", domain.model.SilentBackendConfig())
          case _ => ??? // not implemented
        }
    }.toMap

    val characterConfigList = elem \ "meta" \ "characterconfig"
    val characterConfigMap = characterConfigList.map { cc =>
      val name = cc \@ "name"
      val defaultSerifColor = Some(cc \@ "serif-color").filterNot(_.isEmpty())
      val tachieUrl = Some(cc \@ "tachie-url").filterNot(_.isEmpty())
      name -> domain.model.CharacterConfig(
        name,
        cc \@ "voice-id",
        defaultSerifColor,
        tachieUrl
      )
    }.toMap

    val defaultBackgroundImage =
      (elem \ "meta" \ "assets" \ "backgroundImage")
        .filter(_.attribute("id").map(_.text).contains("default"))
        .headOption
        .flatMap(_.attribute("url").headOption.map(_.text))

    val defaultFont = (elem \ "meta" \ "font").headOption.map(_.text)

    // 発音調整などに使う文字列辞書。今のところVOICEVOXの発音辞書に使っている
    // (word, pronounce, accent lower point)
    val dict: Seq[(String, String, Int)] = util.Dict.dictFromNode(elem)

    val codes: Map[String, (String, Option[String])] =
      (elem \ "predef" \ "code")
        .flatMap(es =>
          es.map { e =>
            val code = e.text.stripLeading()
            val id = e \@ "id"
            val lang = Some(e \@ "lang").filterNot(_.isEmpty())
            id -> (code, lang)
          }
        )
        .toMap

    val maths: Map[String, String] = (elem \ "predef" \ "math")
      .flatMap(es =>
        es.map { e =>
          val math = e.text.stripLeading()
          val id = e \@ "id"

          id -> math
        }
      )
      .toMap

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

  private def generateSilence(
      ctx: Context
  ): IO[(fs2.io.file.Path, FiniteDuration, domain.model.VowelSeqWithDuration)] =
    for {
      len <- IO.pure(
        ctx.silentLength.getOrElse(FiniteDuration(3, "second"))
      ) // 指定してないなら3秒にしているが理由はない
      sha1Hex <- sha1HexCode(len.toString.getBytes)
      path <- IO.pure(os.Path(s"${os.pwd}/artifacts/silence_$sha1Hex.wav"))
      wav <- backgroundIndicator("Exporting silent .wav file").use { _ =>
        ffmpeg.generateSilentWav(path, len)
      }
    } yield (fs2.io.file.Path(path.toString()), len, Seq())

  private def backgroundIndicator(
      message: String
  ): cats.effect.ResourceIO[IO[cats.effect.OutcomeIO[Unit]]] =
    indicator(message).background

  private def indicator(message: String): IO[Unit] =
    piece(s"⢄ $message") *> piece(s"⠢ $message") *> piece(
      s"⠑ $message"
    ) *> piece(s"⡈ $message") foreverM

  private def piece(s: String): IO[Unit] =
    IO.sleep(100 milliseconds) *> IO.print(
      s"\r${withColor(scala.io.AnsiColor.GREEN ++ scala.io.AnsiColor.BOLD)(s)}"
    )

  private def generateSay(
      sayElem: domain.model.Say,
      voiceVox: VoiceVox,
      ctx: Context
  ): IO[
    (
        fs2.io.file.Path,
        scala.concurrent.duration.FiniteDuration,
        domain.model.VowelSeqWithDuration
    )
  ] = for {
    actualPronunciation <- IO.pure(
      ctx.sic.getOrElse(sayElem.text)
    ) // sicがない場合は元々のセリフを使う
    aq <- backgroundIndicator("Building Audio Query").use { _ =>
      // by属性がないことはないやろという想定でgetしている
      audioQueryFetcher.fetch(
        actualPronunciation,
        ctx.spokenByCharacterId.get,
        ctx
      )
    }
    _ <- logger.debug(aq.toString())
    aq <- ctx.speed map (sp => voiceVox.controlSpeed(aq, sp)) getOrElse (IO
      .pure(aq))
    wav <- backgroundIndicator("Synthesizing wav").use { _ =>
      wavGenerator.execute(aq, ctx.spokenByCharacterId.get, ctx)
    }
    sha1Hex <- sha1HexCode(sayElem.text.getBytes())
    path <- backgroundIndicator("Exporting .wav file").use { _ =>
      writeStreamToFile(wav, s"artifacts/voice_${sha1Hex}.wav")
    }
    dur <- ffmpeg.getWavDuration(path.toString)
    vowels <- voiceVox.getVowels(aq)
  } yield (path, dur, vowels)

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
    htmlBuilder.build(serif, ctx)
  }

  val chromiumCommand =
    sys.env.get("CHROMIUM_CMD").getOrElse(config.getString("chromium.command"))

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

  val chromiumNoSandBox = sys.env
    .get("CHROMIUM_NOSANDBOX")
    .map(_ == "1")
    .getOrElse(config.getBoolean("chromium.nosandbox"))
}

package com.github.windymelt.zmm.application

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.effect.std.Mutex
import com.github.windymelt.zmm.application.movieGenaration.{AudioQueryFetcher, DictionaryApplier, Html, IndicatorHelper, WavGenerator, XmlUtil}
import com.github.windymelt.zmm.domain.model.{Context, GeneratedWav, Say, Tachie, TachiePresets, VoiceBackendConfig, VoiceVoxBackendConfig, character}
import com.github.windymelt.zmm.{domain, infrastructure, util}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import fs2.io.file.Path

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.language.postfixOps
import cats.syntax.parallel.*

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
      sayCtxPairs <- IO.pure {
        Context.sayContextPairFromNode(
          xmlUtil.extractDialogElements(x),
          defaultCtx
        )
      }
      // dialogue要素から取得したセリフを元に音声ファイルを作成する
      generatedWavs <- {
        val saySeq = sayCtxPairs map { (s, ctx) =>
          if (ctx.isSilent) wavGenerator.generateSilence(ctx)
          else wavGenerator.generateSay(s, ctx)
        }
        saySeq.parSequence
      }
      // 読み上げ長をContextに追加する。母音情報が得られた場合も追加する
      sayCtxPairs <- IO.pure {
        val pairs = sayCtxPairs zip generatedWavs
        pairs map { (sayCtxPair, generatedWav) =>
          {
            val say = sayCtxPair._1
            val ctx = sayCtxPair._2

            if (generatedWav.isSilent)
              (say, ctx.copy(duration = Some(generatedWav.duration)))
            else
              (
                say,
                ctx.copy(
                  spokenVowels = Some(
                    generatedWav.moras.map(m => (m.vowel, m.finiteDuration))
                  ), // spokenVowelsもmorasに置き換えたいが影響範囲が大きいので今は諦める
                  duration = Some(generatedWav.duration)
                )
              )
          }
        }
      }
      // Contextにフィルタを適用する。母音情報を元に立ち絵を差し替えたコンテキストに分割したりする
      sayCtxPairs <- IO.pure(applyFilters(sayCtxPairs))
      concatenatedSayWav <- ffmpeg.concatenateWavFiles(
        generatedWavs.map(_.path.toString)
      )
      // この時点でvideoとaudioとの間に依存がないので並列実行する
      // BUG: SI-5589 により、タプルにバインドできない
      va <- backgroundIndicator("Generating video and concatenated audio").use {
        _ =>
          val wavPaths = generatedWavs.map(_.path)
          generateVideo(sayCtxPairs, wavPaths) product ffmpeg
            .concatenateWavFiles(wavPaths.map(_.toString))
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
    val tachiePresetsMap = Map.from(characterConfigMap.map { (k, v) => (k, Tachie.prepare(v.tachieUrl.get))})
    val defaultBackgroundImage = xmlUtil.extractDefaultBackgroundImage(elem)
    val defaultFont = xmlUtil.extractDefaultFont(elem)
    // 発音調整などに使う文字列辞書。今のところVOICEVOXの発音辞書に使っている
    // (word, pronounce, accent lower point)
    val dict = xmlUtil.extractPronounceDict(elem)
    val codes: Map[String, (String, Option[String])] =
      xmlUtil.extractCodes(elem)
    val maths = xmlUtil.extractMaths(elem)
    val charactersMap = characterConfigMap.map {
      (k, v) =>
      k -> character.Character(
        character.Config(k, voiceConfigMap(v.voiceId).asInstanceOf[VoiceVoxBackendConfig].speakerId, Tachie.prepare(v.tachieUrl.get)),
        character.State.default
      )
    }

    IO.pure(
      domain.model.Context(
        tachiePresetsMap,
        voiceConfigMap,
        characterConfigMap,
        defaultBackgroundImage,
        dict = dict,
        codes = codes,
        maths = maths,
        font = defaultFont,
        charactersMap = charactersMap
      )
    )
  }

  private def applyFilters(
      pairs: Seq[(domain.model.Say, Context)]
  ): Seq[(domain.model.Say, Context)] = {
    // フィルタが増えたら合成して伸ばす
    // Arrow.secondを使うとタプルの右側だけflatMapし、左側を補完させることができる
    val composedFilters = domain.model.Filter.talkingMouthFilter
    val eyeTransitionFilter = domain.model.Filter.eyeTransitionFilter
    pairs
      .flatMap(composedFilters.second.run)
      .flatMap(eyeTransitionFilter.second.run)
  }

  private def screenShot(
      ss: ScreenShot,
      say: Say,
      ctx: Context
  ): IO[os.Path] = {
    for {
      html <- Html.build(say.text, ctx)
      // スクリーンショットは重いのでHTMLの内容をもとにキャッシュする(HTMLが同一内容なら同一のスクリーンショットになるという前提)
      _ <- html.saveIfNotExist
      screenShotFilePath <- html.screenShotExists.ifM(
        IO.pure(os.pwd / os.RelPath(html.screenShotPath)),
        ss.takeScreenShot(os.pwd / os.RelPath(html.path))
      )
    } yield screenShotFilePath
  }

  private def generateVideo(
      sayCtxPairs: Seq[(Say, Context)],
      sayWavPaths: Seq[fs2.io.file.Path]
  ): IO[os.Path] = {
    for {
      ss <- screenShotResource
      imgs <- for {
        sceneImages: Seq[os.Path] <- sayCtxPairs.map { pair =>
          ss.use { ss =>
            {
              val (say, ctx) = pair
              screenShot(ss, say, ctx)
            }
          }
        }.parSequence
        concatenatedImages <- ffmpeg.concatenateImagesWithDuration(
          sceneImages.zip(sayCtxPairs.map(_._2.duration.get))
        )
      } yield concatenatedImages

    } yield imgs
  }
}

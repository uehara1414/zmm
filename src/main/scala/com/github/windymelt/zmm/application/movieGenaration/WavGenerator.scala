package com.github.windymelt.zmm.application.movieGenaration

import cats.effect.IO
import com.github.windymelt.zmm.domain.model.speech.SpeechParameters
import com.github.windymelt.zmm.domain.model.{Context, GeneratedWav}
import com.github.windymelt.zmm.{domain, infrastructure, util}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.FiniteDuration

class WavGenerator(logLevel: String = "INFO")
    extends domain.repository.VoiceVoxComponent
    with infrastructure.VoiceVoxComponent
    with util.UtilComponent
    with domain.repository.FFmpegComponent
    with infrastructure.FFmpegComponent {
  val voiceVoxUri =
    sys.env.get("VOICEVOX_URI") getOrElse config.getString("voicevox.apiUri")

  // 指定してないなら3秒にしているが理由はない
  val defaultSilentLength = FiniteDuration(3, "second")
  private def audioQueryFetcher = new AudioQueryFetcher()
  implicit def logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  def ffmpeg =
    new ConcreteFFmpeg(
      config.getString("ffmpeg.command"),
      verbosity = logLevel match {
        case "DEBUG" => ConcreteFFmpeg.Verbose
        case "TRACE" => ConcreteFFmpeg.Verbose
        case _       => ConcreteFFmpeg.Quiet
      }
    ) // TODO: respect construct parameter

  def voiceVox: VoiceVox = new ConcreteVoiceVox(voiceVoxUri)

  def execute(
               aq: SpeechParameters,
               character: String,
               ctx: Context
  ): IO[fs2.Stream[IO, Byte]] = {
    val speakerId = extractSpeakerId(character, ctx)

    voiceVox.synthesis(aq, speakerId)
  }

  def generateSilence(ctx: Context): IO[GeneratedWav] =
    for {
      len <- IO.pure(
        ctx.silentLength.getOrElse(defaultSilentLength)
      )
      sha1Hex <- sha1HexCode(len.toString.getBytes)
      path <- IO.pure(os.Path(s"${os.pwd}/artifacts/silence_$sha1Hex.wav"))
      // CLI出力まで持ってくるのがだるいので一旦コメントアウト
      // wav <- backgroundIndicator("Exporting silent .wav file").use { _ =>
      wav <- ffmpeg.generateSilentWav(path, len)
    } yield GeneratedWav(fs2.io.file.Path(path.toString()), len, Seq())

  def generateSay(sayElem: domain.model.Say, ctx: Context): IO[GeneratedWav] =
    for {
      actualPronunciation <- IO.pure(
        ctx.sic.getOrElse(sayElem.text)
      ) // sicがない場合は元々のセリフを使う
      // CLI出力まで持ってくるのがだるいので一旦コメントアウト
      // aq <- backgroundIndicator("Building Audio Query").use { _ =>
      speech <- audioQueryFetcher.fetch( // by属性がないことはないやろという想定でgetしている
          actualPronunciation,
          ctx.spokenByCharacterId.get,
          ctx
        )
      _ <- logger.debug(speech.toString())
      aq <- ctx.speed map (sp => voiceVox.controlSpeed(speech, sp)) getOrElse (IO.pure(speech))
      // CLI出力まで持ってくるのがだるいので一旦コメントアウト
      // wav <- backgroundIndicator("Synthesizing wav").use { _ =>
      wav <- execute(aq, ctx.spokenByCharacterId.get, ctx)
      sha1Hex <- sha1HexCode(sayElem.text.getBytes())
      // CLI出力まで持ってくるのがだるいので一旦コメントアウト
      // path <- backgroundIndicator("Exporting .wav file").use { _ =>
      path <- writeStreamToFile(wav, s"artifacts/voice_${sha1Hex}.wav")
      dur <- ffmpeg.getWavDuration(path.toString)
      vowels <- voiceVox.getVowels(aq)
    } yield GeneratedWav(path, dur, vowels)

  private def extractSpeakerId(character: String, ctx: Context): String = {
    val characterConfig = ctx.characterConfigMap(character)
    val voiceConfig = ctx.voiceConfigMap(characterConfig.voiceId)

    voiceConfig.asInstanceOf[domain.model.VoiceVoxBackendConfig].speakerId
  }
}

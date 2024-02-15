package com.github.windymelt.zmm.application.movieGenaration

import cats.effect.IO
import com.github.windymelt.zmm.domain.model.Context
import com.github.windymelt.zmm.{domain, infrastructure, util}

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

  def ffmpeg =
    new ConcreteFFmpeg(
      config.getString("ffmpeg.command"),
      verbosity = logLevel match {
        case "DEBUG" => ConcreteFFmpeg.Verbose
        case "TRACE" => ConcreteFFmpeg.Verbose
        case _ => ConcreteFFmpeg.Quiet
      }
    ) // TODO: respect construct parameter

  def voiceVox: VoiceVox = new ConcreteVoiceVox(voiceVoxUri)

  def execute(
      aq: AudioQuery,
      character: String,
      ctx: Context
  ): IO[fs2.Stream[IO, Byte]] = {
    val speakerId = extractSpeakerId(character, ctx)

    voiceVox.synthesis(aq, speakerId)
  }
  def generateSilence(
      ctx: Context
  ): IO[(fs2.io.file.Path, FiniteDuration, domain.model.VowelSeqWithDuration)] =
    for {
      len <- IO.pure(
        ctx.silentLength.getOrElse(defaultSilentLength)
      )
      sha1Hex <- sha1HexCode(len.toString.getBytes)
      path <- IO.pure(os.Path(s"${os.pwd}/artifacts/silence_$sha1Hex.wav"))
      // CLI出力まで持ってくるのがだるいので一旦コメントアウト
      // wav <- backgroundIndicator("Exporting silent .wav file").use { _ =>
      wav <- ffmpeg.generateSilentWav(path, len)
    } yield (fs2.io.file.Path(path.toString()), len, Seq())

  private def extractSpeakerId(character: String, ctx: Context): String = {
    val characterConfig = ctx.characterConfigMap(character)
    val voiceConfig = ctx.voiceConfigMap(characterConfig.voiceId)

    voiceConfig.asInstanceOf[domain.model.VoiceVoxBackendConfig].speakerId
  }
}

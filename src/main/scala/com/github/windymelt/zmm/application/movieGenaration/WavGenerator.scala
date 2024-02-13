package com.github.windymelt.zmm.application.movieGenaration

import cats.effect.IO
import com.github.windymelt.zmm.domain.model.Context
import com.github.windymelt.zmm.{domain, infrastructure, util}

class WavGenerator
    extends domain.repository.VoiceVoxComponent
    with infrastructure.VoiceVoxComponent
    with util.UtilComponent {
  val voiceVoxUri =
    sys.env.get("VOICEVOX_URI") getOrElse config.getString("voicevox.apiUri")

  def voiceVox: VoiceVox = new ConcreteVoiceVox(voiceVoxUri)

  def execute(
      aq: AudioQuery,
      character: String,
      ctx: Context
  ): IO[fs2.Stream[IO, Byte]] = {
    val speakerId = extractSpeakerId(character, ctx)

    voiceVox.synthesis(aq, speakerId)
  }

  private def extractSpeakerId(character: String, ctx: Context): String = {
    val characterConfig = ctx.characterConfigMap(character)
    val voiceConfig = ctx.voiceConfigMap(characterConfig.voiceId)

    voiceConfig.asInstanceOf[domain.model.VoiceVoxBackendConfig].speakerId
  }
}

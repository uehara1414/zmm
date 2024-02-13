package com.github.windymelt.zmm.application.movieGenaration

import cats.effect.IO
import com.github.windymelt.zmm.domain.model.Context
import com.github.windymelt.zmm.domain
import com.github.windymelt.zmm.{infrastructure, util}


class AudioQueryFetcher extends domain.repository.VoiceVoxComponent
  with infrastructure.VoiceVoxComponent
  with util.UtilComponent{
  val voiceVoxUri =
    sys.env.get("VOICEVOX_URI") getOrElse config.getString("voicevox.apiUri")

  def voiceVox: VoiceVox = new ConcreteVoiceVox(voiceVoxUri)

  // Contextに依存したくないので外す。あとで。
  def fetch(text: String, character: String, ctx: Context): IO[AudioQuery] = {
    val speakerId = extractSpeakerId(character, ctx)

    voiceVox.audioQuery(text, speakerId)
  }

  private def extractSpeakerId(character: String, ctx: Context): String = {
    val characterConfig = ctx.characterConfigMap(character)
    val voiceConfig = ctx.voiceConfigMap(characterConfig.voiceId)

    voiceConfig.asInstanceOf[domain.model.VoiceVoxBackendConfig].speakerId
  }
}

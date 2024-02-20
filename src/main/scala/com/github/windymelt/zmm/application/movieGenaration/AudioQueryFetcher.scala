package com.github.windymelt.zmm.application.movieGenaration

import cats.effect.IO
import com.github.windymelt.zmm.domain.model.Context
import com.github.windymelt.zmm.domain
import com.github.windymelt.zmm.domain.model.character.Character
import com.github.windymelt.zmm.domain.model.speech.SpeechParameters
import com.github.windymelt.zmm.{infrastructure, util}


class AudioQueryFetcher extends domain.repository.VoiceVoxComponent
  with infrastructure.VoiceVoxComponent
  with util.UtilComponent{
  val voiceVoxUri =
    sys.env.get("VOICEVOX_URI") getOrElse config.getString("voicevox.apiUri")

  def voiceVox: VoiceVox = new ConcreteVoiceVox(voiceVoxUri)

  // Contextに依存したくないので外す。あとで。
  def fetch(text: String, character: Character): IO[SpeechParameters] = {
    voiceVox.audioQuery(text, character.config.speakerId)
  }
}

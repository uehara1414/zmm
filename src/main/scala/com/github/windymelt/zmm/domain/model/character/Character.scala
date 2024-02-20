package com.github.windymelt.zmm.domain.model.character

import com.github.windymelt.zmm.domain.model.*

sealed trait TachiePosition

object TachiePosition {
  case object Left extends TachiePosition
  case object Right extends TachiePosition
}

case class State(mouthShape: MouthShape, eyeState: EyeState, isSpeaking: Boolean = false, display: Boolean = false, position: TachiePosition = TachiePosition.Right)
case class Config(characterId: String, speakerId: String, presets: TachiePresets, serifColor: Option[String])

case class Character(config: Config, state: State) {
  def tachieUrl: String = {
    Tachie.getTachie(
      state.mouthShape,
      state.eyeState,
      config.presets
    ).tachieUrl
  }
}

object State {
  def default: State = State(MouthShape.default, EyeState.default)
}

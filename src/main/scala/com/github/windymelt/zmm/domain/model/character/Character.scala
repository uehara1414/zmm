package com.github.windymelt.zmm.domain.model.character

import com.github.windymelt.zmm.domain.model.*

case class State(mouthShape: MouthShape, eyeState: EyeState)
case class Config(characterId: String, speakerId: String, presets: TachiePresets)

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

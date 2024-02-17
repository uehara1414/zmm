package com.github.windymelt.zmm.domain.model

case class EyeState(openState: OpenState)

sealed trait OpenState

object OpenState {
  case object Open extends OpenState
  case object Close extends OpenState
}

object EyeState {
  val default: EyeState = EyeState(OpenState.Open)

  val eyeStates: List[EyeState] = List(
    default,

    EyeState(OpenState.Close)
  )

  val fallbackRules: Map[EyeState, EyeState] = Map(
    EyeState(OpenState.Close) -> EyeState(OpenState.Open)
  )
}

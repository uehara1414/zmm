package com.github.windymelt.zmm
package domain.repository

import cats.effect.IO

trait CoeiroInkComponent {
  type Speakers
  def coeiroInk: CoeiroInk

  trait CoeiroInk {
    def speakers: IO[Speakers]
  }
}

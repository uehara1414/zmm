package com.github.windymelt.zmm
package infrastructure

import cats.effect.IO

case class Speaker(
  name: String
)

trait CoeiroInkComponent {
  self: domain.repository.CoeiroInkComponent =>
  type Speakers = Seq[Speaker]

  def coeiroInk: CoeiroInk

  class ConcreteCoeiroInk extends CoeiroInk:
    def speakers: IO[Speakers] =
      for {
        _ <- IO.println("CoeiroInkComponent.ConcreteCoeiroInk.speakers")
        speakers = Seq(Speaker("つくよみちゃん in local"), Speaker("黄琴海月 in local"))
      } yield speakers
}

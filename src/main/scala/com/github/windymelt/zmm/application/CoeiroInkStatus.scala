package com.github.windymelt.zmm.application

import cats.effect.IO
import com.github.windymelt.zmm.{domain, infrastructure}
import com.github.windymelt.zmm.infrastructure.CoeiroInkComponent

class CoeiroInkStatus extends infrastructure.CoeiroInkComponent
    with domain.repository.CoeiroInkComponent:

  def coeiroInk = new ConcreteCoeiroInk()

  def show(): IO[Unit] =
    for {
      _ <- IO.println("hogehoge in CoeiroInkStatus.show")
      speakers <- coeiroInk.speakers
      _ <- IO.println(s"speakers: $speakers")
      _ <- IO.println("CoeiroInkStatus.show finish")
    } yield ()

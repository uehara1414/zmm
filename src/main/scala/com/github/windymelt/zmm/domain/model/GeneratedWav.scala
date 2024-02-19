package com.github.windymelt.zmm.domain.model

import fs2.io.file.Path
import com.github.windymelt.zmm.domain.model.speech.Mora

import scala.concurrent.duration.FiniteDuration

case class GeneratedWav (path: Path, duration: FiniteDuration, moras: Seq[Mora]) {
  def isSilent: Boolean = moras == Seq()
}


package com.github.windymelt.zmm.domain.model

import fs2.io.file.Path

import scala.concurrent.duration.FiniteDuration

case class GeneratedWav (path: Path, duration: FiniteDuration, vowelSeqWithDuration: VowelSeqWithDuration) {
  def isSilent: Boolean = vowelSeqWithDuration == Seq()
}


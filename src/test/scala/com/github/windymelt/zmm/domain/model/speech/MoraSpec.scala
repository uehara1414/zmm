package com.github.windymelt.zmm.domain.model.speech

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MoraSpec extends AnyFlatSpec with Matchers {
  "Mora" should "生成できる" in {
    Mora("コ", "k", 0.1, "o", 0.2, 1.0) shouldEqual Mora("コ", "k", 0.1, "o", 0.2, 1.0)
  }
}


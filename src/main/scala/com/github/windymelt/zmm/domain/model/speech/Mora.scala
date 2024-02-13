package com.github.windymelt.zmm.domain.model.speech

case class Mora(
    text: String,
    consonant: String,
    consonantLength: Double,
    vowel: String,
    vowelLength: Double,
    pitch: Double
)



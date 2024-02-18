package com.github.windymelt.zmm.domain.model

case class MouthShape(shape: String)

object MouthShape {
  val default: MouthShape = MouthShape("")

  val mouthShapes: List[MouthShape] = List(
    default,

    MouthShape("a"),
    MouthShape("i"),
    MouthShape("u"),
    MouthShape("e"),
    MouthShape("o"),

    MouthShape("cl"),
    MouthShape("n"),
  )

  val mouthShapeByVowel: Map[String, MouthShape] = Map(
    "a" -> MouthShape("a"),
    "i" -> MouthShape("i"),
    "u" -> MouthShape("u"),
    "e" -> MouthShape("e"),
    "o" -> MouthShape("o"),
    "A" -> MouthShape("a"),
    "I" -> MouthShape("i"),
    "U" -> MouthShape("u"),
    "E" -> MouthShape("e"),
    "O" -> MouthShape("o"),
    "ッ" -> MouthShape("cl"),
    "N" -> MouthShape("n"),
    // 他にある？
  )

  val fallbackRules: Map[MouthShape, MouthShape] = Map(
    // 「あいうえお」のみへのフォールバック
    MouthShape("cl") -> MouthShape("u"),
    MouthShape("n") -> default,

    // 母音がない場合のフォールバック
    MouthShape("a") -> default,
    MouthShape("i") -> default,
    MouthShape("u") -> default,
    MouthShape("e") -> default,
    MouthShape("o") -> default,
  )
}

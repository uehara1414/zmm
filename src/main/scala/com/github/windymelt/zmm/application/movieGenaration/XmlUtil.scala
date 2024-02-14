package com.github.windymelt.zmm.application.movieGenaration

import cats.effect.IO

// とりあえずここにxml関連の処理を凝集させてみる。その後に責務を考える
class XmlUtil {
  def sanitize(elem: scala.xml.Elem): IO[Unit] = {
    val checkTopElem = elem.label == "content"
    val ver = elem \@ "version" == "0.0"

    if (!(checkTopElem && ver)) {
      throw new Exception("Invalid scenary XML") // TODO: 丁寧なエラーメッセージ
    }
    IO.unit
  }
}

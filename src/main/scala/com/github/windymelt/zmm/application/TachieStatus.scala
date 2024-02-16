package com.github.windymelt.zmm.application

import cats.effect.IO

class TachieStatus {
  val tachieUrl = "../../assets/黄琴海月.png"

  /** 立ち絵関連の状態を表示する
    * @return
    *   IO[Unit]
    */
  def execute(): IO[Unit] = {
    val ExtRe = """(.+)\.(.+)""".r.anchored

    val alternate = tachieUrl match {
      case originalPath @ ExtRe(file: String, ext: String) =>
        s"${file}!.$ext"
      case _ => ???
    }

    IO.println(s"WIP 立ち絵を表示するよ") >>
      IO.println(s"${tachieUrl}") >>
      IO.println(s"${alternate}")
  }
}

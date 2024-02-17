package com.github.windymelt.zmm.application

import cats.effect.IO
import com.github.windymelt.zmm.domain.model.{EyeState, MouthShape, Tachie}


class TachieStatus {
  val tachieUrl = "@assets/黄琴海月.png"

  /** 立ち絵関連の状態を表示する
    * @return
    *   IO[Unit]
    */
  def execute(): IO[Unit] = {
    val ExtRe = """(.+)\.(.+)""".r.anchored
    val preset = Tachie.prepare(tachieUrl)

    IO.println(s"黄琴海月の立ち絵の準備状況") >>
      IO.println(s"${preset.tachies.mkString("\n")}") >>
      IO.println(s"${os.pwd}")
  }
}

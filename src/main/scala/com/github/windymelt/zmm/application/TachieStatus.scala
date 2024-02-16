package com.github.windymelt.zmm.application

import cats.effect.IO

class TachieStatus {

  /** 立ち絵関連の状態を表示する
    * @return
    *   IO[Unit]
    */
  def execute(): IO[Unit] = {
    IO.print(s"WIP 立ち絵を表示するよ") >>
      IO.println("")
  }
}

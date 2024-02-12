package com.github.windymelt.zmm.application

import cats.effect.IO
import com.github.windymelt.zmm.{BuildInfo}

class ShowVersion {
  /** ZMMのバージョンを表示する。
   *
   * デバッグや問い合わせの助けとしても使う。
   *
   * @return
   * IO[Unit]
   */
  def execute(): IO[Unit] = {
    IO.print("zmm ver=") >>
      (BuildInfo.version match {
        case s"$_-SNAPSHOT" =>
          IO.print(withColor(scala.io.AnsiColor.YELLOW)(BuildInfo.version))
        case _ =>
          IO.print(withColor(scala.io.AnsiColor.GREEN)(BuildInfo.version))
      }) >>
      ((System.getenv("IS_DOCKER_ZMM") == "1") match {
        case true => IO.print(withColor(scala.io.AnsiColor.CYAN)(" (Docker)"))
        case false => IO.unit
      }) >>
      IO.print(", scalaVer=") >>
      IO.print(withColor(scala.io.AnsiColor.GREEN)(BuildInfo.scalaVersion)) >>
      IO.print(", sbtVer=") >>
      IO.print(withColor(scala.io.AnsiColor.GREEN)(BuildInfo.sbtVersion)) >>
      IO.print(s", jvm=${System.getProperty("java.vm.name")}") >>
      IO.print(s", runtimeVer=${Runtime.version().toString()}") >>
      IO.print(s", vendor=${System.getProperty("java.vendor")}") >>
      IO.println("")
  }

  private def withColor(color: String) = (s: String) =>
    s"${color.toString()}${s}${scala.io.AnsiColor.RESET}"

}

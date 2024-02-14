package com.github.windymelt.zmm.application.movieGenaration

import cats.effect.IO

import scala.concurrent.duration.DurationInt
import scala.language.postfixOps


trait IndicatorHelper {
  def withColor(color: String): String => String = (s: String) =>
    s"${color.toString()}${s}${scala.io.AnsiColor.RESET}"

  def backgroundIndicator(
                                   message: String
                                 ): cats.effect.ResourceIO[IO[cats.effect.OutcomeIO[Unit]]] =
    indicator(message).background

  private def indicator(message: String): IO[Unit] =
    piece(s"⢄ $message") *> piece(s"⠢ $message") *> piece(
      s"⠑ $message"
    ) *> piece(s"⡈ $message") foreverM

  private def piece(s: String): IO[Unit] =
    IO.sleep(100 milliseconds) *> IO.print(
      s"\r${withColor(scala.io.AnsiColor.GREEN ++ scala.io.AnsiColor.BOLD)(s)}"
    )
}

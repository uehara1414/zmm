package com.github.windymelt.zmm

import cats.effect.IO
import com.github.windymelt.zmm.application.GenerateMovie
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class ChromiumCli(logLevel: String = "INFO") {
  implicit def logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  def generate(file: String, out: String): IO[Unit] = {
    val movieGenerator = new GenerateMovie(file, out)
    movieGenerator.execute
  }
}

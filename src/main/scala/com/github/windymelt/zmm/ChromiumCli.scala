package com.github.windymelt.zmm

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.effect.std.Mutex
import cats.implicits._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class ChromiumCli(logLevel: String = "INFO") {
  implicit def logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  def generate(file: String, out: String): IO[Unit] = {
    for {
      _ <- logger.debug(s"Generating $out from $file")
      _ <- logger.debug(s"Log level: $logLevel")
    } yield ()
  }
}

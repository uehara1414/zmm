package com.github.windymelt.zmm.application.movieGenaration

import cats.effect.IO
import com.github.windymelt.zmm.domain.model.Context
import com.github.windymelt.zmm.util.UtilComponent

case class Html(body: String) extends UtilComponent {
  def sha1HexCode: IO[String] = {
    sha1HexCode(body.getBytes())
  }

  // Notice: 参照先の画像などを差し替えてもhashは変わらないので画像が更新されないことがある
  def path: String = {
    s"./artifacts/html/${sha1HexCode}.html"
  }

  def saveIfNotExist: IO[fs2.io.file.Path] = {
    checkfileExists(path).ifM(
      IO.pure(fs2.io.file.Path(path)),
      writeStreamToFile(fs2.Stream[IO, Byte](body.getBytes().toSeq: _*), path)
    )
  }

  def screenShotPath: String = {
    s"${path}.png"
  }

  def screenShotExists: IO[Boolean] = {
    checkfileExists(screenShotPath)
  }

  private def checkfileExists: String => IO[Boolean] = p =>
    IO(os.exists(os.pwd / os.RelPath(p)))
}

class HtmlBuilder {
  def build(serif: String,
            ctx: Context,
            debuggingInfo: Seq[String] = Seq("文字列を動画中に表示します", "デバッグに使いましょう")): IO[Html] = {
    IO {
      val htmlBody = html.sample(serif = serif, ctx = ctx, debuggingInfo = debuggingInfo).body

      Html(htmlBody)
    }
  }
}

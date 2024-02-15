package com.github.windymelt.zmm.application.movieGenaration

import cats.effect.IO
import com.github.windymelt.zmm.domain.model.Context


class HtmlBuilder {
  def build(serif: String, ctx: Context, debuggingInfo: Seq[String] = Seq("文字列を動画中に表示します", "デバッグに使いましょう")): IO[String] = {
    IO {
      html.sample(serif = serif, ctx = ctx, debuggingInfo = debuggingInfo).body
    }
  }
}

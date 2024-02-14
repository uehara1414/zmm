package com.github.windymelt.zmm.application.movieGenaration

import cats.effect.IO
import com.github.windymelt.zmm.domain.model.Context


class HtmlBuilder {
  def build(serif: String, ctx: Context): IO[String] = {
    IO {
      html.sample(serif = serif, ctx = ctx).body
    }
  }
}

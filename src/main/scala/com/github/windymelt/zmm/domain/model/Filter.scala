package com.github.windymelt.zmm
package domain.model

import cats.data.Kleisli
import cats.implicits.*

object Filter {
  def talkingMouthFilter: Filter[Seq] = Kleisli { (ctx: Context) =>
    val tachiePresetsByCharacterId = ctx.characterConfigMap.view.mapValues(config =>
      Tachie.prepare(config.tachieUrl.getOrElse(""))
    )

    ctx.spokenVowels match {
      case None => Seq(ctx)
      case Some(vs) =>
        val ExtRe = """(.+)\.(.+)""".r.anchored

        val spokenCtxs = vs.map { v =>
          ctx.spokenByCharacterId match {
            case None => ctx
            case Some(cid) =>
              ctx.copy(
                duration = Some(v._2),
                currentVowel = Some("vowel: %s".format(v._1)),
                tachieUrl = Some(Tachie.getTachieFromVowel(v._1, tachiePresetsByCharacterId(ctx.spokenByCharacterId.get)).tachieUrl)
              )
          }
        }

        // 合計Durationは元々のDurationと一致させるべく調整する
        val diff = ctx.duration.get - (vs.map(_._2).combineAll)
        val size = spokenCtxs.size
        val acc = diff / size

        spokenCtxs.map(c => c.copy(duration = c.duration.map(_ + acc)))
    }
  }
}

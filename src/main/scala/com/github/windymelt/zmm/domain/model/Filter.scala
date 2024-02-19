package com.github.windymelt.zmm
package domain.model

import cats.data.Kleisli
import cats.implicits.*

import scala.concurrent.duration.FiniteDuration

object Filter {
  def talkingMouthFilter: Filter[Seq] = Kleisli { (ctx: Context) =>
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
                currentVowel = Some(v._1),
                charactersMap = ctx.charactersMap // こんなネストが必要になるのは設計が間違ってるんだと思うが、今の経験値ではどうせ大した設計はできないのでしばらくこれでいく
                  .updated(
                    cid,
                    ctx.speakingCharacter.copy(state =
                      ctx.speakingCharacter.state
                        .copy(mouthShape = MouthShape.mouthShapeByVowel(v._1))
                    )
                  )
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

  def eyeTransitionFilter: Filter[Seq] = Kleisli { (ctx: Context) =>
    {
      ctx.currentVowel match {
        case None => Seq(ctx)
        case Some(v) =>
          v match {
            // 母音が「う」の時に瞬きするようにしてみる。
            case "u" =>
              Seq(
                ctx.copy(
                  duration = Some(ctx.duration.get / 2),
                  charactersMap = ctx.charactersMap
                    .updated(
                      ctx.spokenByCharacterId.get,
                      ctx.speakingCharacter.copy(state =
                        ctx.speakingCharacter.state
                          .copy(eyeState = EyeState(OpenState.Close))
                      )
                    )
                ),
                ctx.copy(
                  charactersMap = ctx.charactersMap
                    .updated(
                      ctx.spokenByCharacterId.get,
                      ctx.speakingCharacter.copy(state =
                        ctx.speakingCharacter.state
                          .copy(eyeState = EyeState(OpenState.Open))
                      )
                    ),
                  duration = Some(ctx.duration.get / 2)
                )
              )
            case _ => Seq(ctx)
          }
      }
    }
  }

}

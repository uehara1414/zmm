package com.github.windymelt.zmm.domain.model

import com.github.windymelt.zmm.domain.model.character.{Character, TachiePosition}
import com.github.windymelt.zmm.domain.model.speech.Mora

import scala.concurrent.duration.FiniteDuration

/*
 Contextの4要素:
 - Context class
 - Contextを合成するためのMonoid instance
 - Elem -> Contextする写像
 - 初期値をroot Elem -> Contextで決定する写像
 これがフィールドごとに一列になっていると便利なのだけれど動的にクラスを組み立てることは(Shapelessなどを使わないかぎり)不可能なので、
 下3つを直列記述するbuilderを作ることを今後のTODOとしたい
 */

final case class Context(
    backgroundImageUrl: Option[String] = None,
    duration: Option[FiniteDuration] = None, // 音声合成時に明らかになるのでデフォルトではNone
    spokenVowels: Option[Seq[(String, FiniteDuration)]] = None, // 口パクのために使う母音情報
    speed: Option[String] = Some("1.0"),
    font: Option[String] = None,
    additionalTemplateVariables: Map[String, String] = Map.empty,
    bgm: Option[String] = None,
    codes: Map[String, (String, Option[String])] =
      Map.empty, // id -> (code, lang?)
    maths: Map[String, String] = Map.empty, // id -> LaTeX string
    sic: Option[String] = None, // 代替読みを設定できる(数式などで使う)
    video: Option[String] = None, // 背景に合成する動画
    currentVowel: Option[String] = None,
    charactersMap: Map[String, Character] = Map.empty[String, Character]
    // TODO: BGM, fontColor, etc.
) {
  def atv: Map[String, String] =
    additionalTemplateVariables // alias for template

  def isSilent: Boolean = !charactersMap.values.exists(_.state.isSpeaking)

  def tachieUrl: Option[String] = rightCharacter.map(c => c.tachieUrl)

  def rightCharacter: Option[Character] = charactersMap.values.find(c =>
    c.state.position == TachiePosition.Right && c.state.display
  )

  def leftCharacter: Option[Character] = charactersMap.values.find(c =>
    c.state.position == TachiePosition.Left && c.state.display
  )

  def leftTachieUrl: Option[String] =
    leftCharacter.flatMap(c => Some(c.tachieUrl))

  def speakingCharacter: Option[Character] = {
    charactersMap.values.find(_.state.isSpeaking)
  }

  def serifColor: String = {
    speakingCharacter.flatMap(c => c.config.serifColor).getOrElse("black")
  }
}

// TODO: 後で動かす
sealed trait DialogueTree

final case class Say(text: String)
final case class Speak(text: String, moras: Seq[Mora])

// TODO: 仮にsceneだけとしている(他にも色々ありそう)
final case class Scene(children: Seq[DialogueTree])

object Context {
  import cats._
  import cats.implicits._
  import scala.xml.{Text, Node, Elem, Comment}

  // Context is a Monoid
  implicit val monoidForContext: Monoid[Context] = new Monoid[Context] {
    def combine(x: Context, y: Context): Context = {
      val xCharacterId: Option[String] =
        x.speakingCharacter.flatMap(xc => Some(xc.config.characterId))
      val yCharacterId: Option[String] =
        y.speakingCharacter.flatMap(yc => Some(yc.config.characterId))
      val speakingCharacterId: Option[String] = xCharacterId orElse yCharacterId
      Context(
        backgroundImageUrl =
          y.backgroundImageUrl orElse x.backgroundImageUrl, // 後勝ち
        duration = y.duration <+> x.duration,
        spokenVowels = y.spokenVowels <+> x.spokenVowels,
        speed = y.speed orElse x.speed, // 後勝ち
        font = y.font orElse x.font, // 後勝ち
        additionalTemplateVariables =
          x.additionalTemplateVariables ++ y.additionalTemplateVariables,
        bgm = y.bgm orElse x.bgm,
        codes =
          x.codes |+| y.codes, // Map の Monoid性を応用すると、同一idで書かれたコードは結合されるという好ましい特性が表われるのでこうしている。additionalTemplateVariablesに畳んでもいいかもしれない。現在のコードはadditionalTemplateVariablesに入れている
        maths = x.maths |+| y.maths,
        sic = y.sic orElse x.sic,
        video = y.video <+> x.video,
        currentVowel = y.currentVowel,
        charactersMap = x.charactersMap ++ y.charactersMap
      )
    }

    def empty: Context = Context.empty
  }

  def sayContextPairFromNode(
                              dialogueElem: scala.xml.Node,
                              currentContext: Context = Context.empty
                            ): Seq[(Say, Context)] = dialogueElem match {
    case Comment(_) => Seq.empty // コメントは無視する
    case Text(t) if t.forall(_.isWhitespace) =>
      Seq.empty // 空行やただの入れ子でコンテキストが生成されないようにする
    case Text(t) => Seq(Say(t) -> currentContext)
    case e: Elem =>
      e.child.flatMap(c =>
        sayContextPairFromNode(c, currentContext |+| extract(e, currentContext))
      )
  }

  private def firstAttrTextOf(e: Elem, a: String): Option[String] =
    e.attribute(a).headOption.flatMap(_.headOption).map(_.text)

  private def extract(e: Elem, ctx: Context): Context = {
    val atvs = {
      val motif = firstAttrTextOf(e, "motif").map("motif" -> _)
      val code = firstAttrTextOf(e, "code").map("code" -> _)
      val math = firstAttrTextOf(e, "math").map("math" -> _)
      Seq(motif, code, math).flatten.toMap
    }

    val by = firstAttrTextOf(e, "by")

    val updatedCharactersMap = by match {
      case Some(id) => {
        val updatedCharacter = ctx
          .charactersMap(id)
          .copy(
            state = ctx.charactersMap(id).state.copy(isSpeaking = true)
          )
        ctx.charactersMap
          .map((k, v) => {
            k -> v.copy(state = v.state.copy(isSpeaking = false))
          })
          .updated(
            id,
            updatedCharacter
          )
      }
      case _ => ctx.charactersMap
    }

    Context(
      backgroundImageUrl =
        firstAttrTextOf(e, "backgroundImage"), // TODO: no camelCase
      speed = firstAttrTextOf(e, "speed"),
      font = firstAttrTextOf(e, "font"),
      additionalTemplateVariables = atvs,
      bgm = firstAttrTextOf(e, "bgm"),
      sic = firstAttrTextOf(e, "sic"),
      duration = firstAttrTextOf(e, "duration").map(l => FiniteDuration.apply(Integer.parseInt(l), "second")),
      video = firstAttrTextOf(e, "video"),
      charactersMap = updatedCharactersMap
    )
  }

  val empty: Context = Context()
}

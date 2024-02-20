package com.github.windymelt.zmm.application.movieGenaration

import cats.effect.IO
import com.github.windymelt.zmm.domain.model.character.TachiePosition
import com.github.windymelt.zmm.domain.model.{CharacterConfig, VoiceBackendConfig, character}
import com.github.windymelt.zmm.{domain, util}

import scala.xml.Elem

// とりあえずここにxml関連の処理を凝集させてみる。その後に責務を考える
class XmlUtil {
  def sanitize(elem: scala.xml.Elem): IO[Unit] = {
    val checkTopElem = elem.label == "content"
    val ver = elem \@ "version" == "0.0"

    if (!(checkTopElem && ver)) {
      throw new Exception("Invalid scenary XML") // TODO: 丁寧なエラーメッセージ
    }
    IO.unit
  }

  def readXml(filePath: String): IO[Elem] = {
    IO.delay(scala.xml.XML.loadFile(filePath))
  }

  def extractDialogElements(elem: Elem): scala.xml.Node = {
    (elem \ "dialogue").head
  }

  def extractVoiceConfigMap(elem: Elem): Map[String, VoiceBackendConfig] = {
    val voiceConfigList = elem \ "meta" \ "voiceconfig"
    voiceConfigList.map { vc =>
      vc \@ "backend" match {
        case "voicevox" =>
          val vvc = vc \ "voicevoxconfig"
          val voiceVoxSpeakerId = vvc \@ "id"
          (vc \@ "id", domain.model.VoiceVoxBackendConfig(voiceVoxSpeakerId))
        case "silent" =>
          (vc \@ "id", domain.model.SilentBackendConfig())
        case _ => ??? // not implemented
      }
    }.toMap
  }

  def extractCharacterConfigMap(elem: Elem): Map[String, CharacterConfig] = {
    val characterConfigList = elem \ "meta" \ "characterconfig"

    characterConfigList.map { cc =>
      val name = cc \@ "name"
      val defaultSerifColor = Some(cc \@ "serif-color").filterNot(_.isEmpty())
      val tachieUrl = Some(cc \@ "tachie-url").filterNot(_.isEmpty())
      val position = Some(cc \@ "position").filterNot(_.isEmpty()) match {
        case Some("left") => Some(TachiePosition.Left)
        case Some("right") => Some(TachiePosition.Right)
        case _ => Some(TachiePosition.Right)
      }
      val display = Some(cc \@ "display").filterNot(_.isEmpty()) match {
        case Some("true") => true
        case Some("false") => false
        case _ => false
      }
      name -> CharacterConfig(
        name,
        cc \@ "voice-id",
        defaultSerifColor,
        tachieUrl,
        position,
        display
      )
    }.toMap
  }

  def extractDefaultBackgroundImage(elem: Elem): Option[String] = {
    (elem \ "meta" \ "assets" \ "backgroundImage")
      .filter(_.attribute("id").map(_.text).contains("default"))
      .headOption
      .flatMap(_.attribute("url").headOption.map(_.text))
  }

  def extractDefaultFont(elem: Elem): Option[String] = {
    (elem \ "meta" \ "font").headOption.map(_.text)
  }

  def extractPronounceDict(elem: Elem): Seq[(String, String, Int)] = {
    util.Dict.dictFromNode(elem)
  }

  def extractCodes(elem: Elem): Map[String, (String, Option[String])] = {
    (elem \ "predef" \ "code")
      .flatMap(es =>
        es.map { e =>
          val code = e.text.stripLeading()
          val id = e \@ "id"
          val lang = Some(e \@ "lang").filterNot(_.isEmpty())
          id -> (code, lang)
        }
      )
      .toMap
  }

  def extractMaths(elem: Elem): Map[String, String] = {
    (elem \ "predef" \ "math")
      .flatMap(es =>
        es.map { e =>
          val math = e.text.stripLeading()
          val id = e \@ "id"

          id -> math
        }
      )
      .toMap
  }
}

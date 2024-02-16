package com.github.windymelt.zmm.domain.model

import com.github.windymelt.zmm.domain.model.MouthShape.fallbackRules
import com.github.windymelt.zmm.util

case class Tachie(mouthShape: MouthShape, tachieUrl: String, available: Boolean)

case class TachiePresets(tachies: Seq[Tachie])

object Tachie {
  import MouthShape.mouthShapes
  import MouthShape.mouthShapeByVowel

  def prepare(baseTachieUrl: String): TachiePresets = {
    val registeredTachies = mouthShapes.map {
      mouthShape => {
        val tachieUrl = buildTachieUrl(mouthShape, baseTachieUrl)
        val existence = tachieUrlExists(tachieUrl)

        Tachie(mouthShape, buildTachieUrl(mouthShape, baseTachieUrl), existence)
      }
    }

    TachiePresets(registeredTachies)
  }

  def getTachieFromVowel(vowel: String, tachiePresets: TachiePresets): Tachie = {
    mouthShapeByVowel.get(vowel) match {
      case Some(mouseShape) => getTachie(mouseShape, tachiePresets)
      case None => tachiePresets.tachies.find(_.mouthShape == MouthShape("")).get
    }
  }

  private def tachieUrlExists(tachieUrl: String): Boolean = {
    val realPath = os.pwd / os.RelPath(util.PathAlias.resolve(tachieUrl, "ffmpeg"))
    os.exists(realPath)
  }

  private def buildTachieUrl(mouseShape: MouthShape, baseTachieUrl: String): String = {
    val ExtRe = """(.+)\.(.+)""".r.anchored

    val suffix = mouseShape match {
      case MouthShape("") => ""
      case MouthShape(_) => s"_${mouseShape.shape}"
    }

    baseTachieUrl match {
      case url @ ExtRe(file: String, ext: String) => s"${file}${suffix}.$ext"
      case _ => ???
    }
  }

  private def getTachie(mouseShape: MouthShape, tachiePresets: TachiePresets): Tachie = {
    val tachie = tachiePresets.tachies.find(_.mouthShape == mouseShape).get
    tachie match {
      case Tachie(_, _, true) => tachie
      case Tachie(_, _, false) => getTachie(fallbackRules(tachie.mouthShape), tachiePresets)
    }
  }
}

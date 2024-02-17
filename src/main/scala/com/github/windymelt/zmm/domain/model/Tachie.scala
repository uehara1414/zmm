package com.github.windymelt.zmm.domain.model

import com.github.windymelt.zmm.domain.model.MouthShape.fallbackRules
import com.github.windymelt.zmm.util

case class Tachie(mouthShape: MouthShape, eyeState: EyeState, tachieUrl: String, available: Boolean)

case class TachiePresets(tachies: Seq[Tachie])

object Tachie {
  import MouthShape.mouthShapes
  import MouthShape.mouthShapeByVowel
  import EyeState.eyeStates

  def prepare(baseTachieUrl: String): TachiePresets = {
    val registeredTachies = (for(mouseShape <- mouthShapes; eyeState <- eyeStates) yield(mouseShape, eyeState)).map {
      (mouthShape, eyeState) => {
        val tachieUrl = buildTachieUrl(mouthShape, eyeState, baseTachieUrl)
        val existence = tachieUrlExists(tachieUrl)

        Tachie(mouthShape, eyeState, buildTachieUrl(mouthShape, eyeState, baseTachieUrl), existence)
      }
    }

    TachiePresets(registeredTachies)
  }

  def getTachieFromVowel(vowel: String, eyeState: EyeState, tachiePresets: TachiePresets): Tachie = {
    mouthShapeByVowel.get(vowel) match {
      case Some(mouthShape) => getTachie(mouthShape, eyeState, tachiePresets)
      case None => tachiePresets.tachies.find(_.mouthShape == MouthShape("") && eyeState == EyeState.default).get
    }
  }

  private def tachieUrlExists(tachieUrl: String): Boolean = {
    val realPath = os.pwd / os.RelPath(util.PathAlias.resolve(tachieUrl, "ffmpeg"))
    os.exists(realPath)
  }

  private def buildTachieUrl(mouseShape: MouthShape, eyeState: EyeState, baseTachieUrl: String): String = {
    val ExtRe = """(.+)\.(.+)""".r.anchored

    val mouseShapeSuffix = mouseShape match {
      case MouthShape("") => ""
      case MouthShape(_) => s"_${mouseShape.shape}"
    }

    val eyeStateSuffix = eyeState match {
      case EyeState(OpenState.Open) => ""
      case EyeState(OpenState.Close) => "_close"
    }

    val suffix = mouseShapeSuffix + eyeStateSuffix

    baseTachieUrl match {
      case url @ ExtRe(file: String, ext: String) => s"${file}${suffix}.$ext"
      case _ => ???
    }
  }

  private def getTachie(mouseShape: MouthShape, eyeState: EyeState, tachiePresets: TachiePresets): Tachie = {
    val tachie = tachiePresets.tachies.find(t => {t.mouthShape == mouseShape && t.eyeState == eyeState}).get
    tachie match {
      case Tachie(_, _, _, true) => tachie
      // todo: 瞬き画像のfallbackもする
      case Tachie(_, _, _, false) => getTachie(fallbackRules(tachie.mouthShape), eyeState, tachiePresets)
    }
  }
}

package com.github.windymelt.zmm.domain.model

import com.github.windymelt.zmm.domain.model.MouthShape.fallbackRules
import com.github.windymelt.zmm.util

import java.awt.MouseInfo

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

  def getTachie(mouseShape: MouthShape, eyeState: EyeState, tachiePresets: TachiePresets): Tachie = {
    val tachie = tachiePresets.tachies.find(t => {t.mouthShape == mouseShape && t.eyeState == eyeState}).get
    tachie match {
      case Tachie(_, _, _, true) => tachie
      case Tachie(_, _, _, false) => {
        val fallbackMouthShape = fallbackRules.get(tachie.mouthShape)
        val fallbackEyeState = EyeState.fallbackRules.get(eyeState)

        (fallbackMouthShape, fallbackEyeState) match {
          case (_, Some(_)) => getTachie(tachie.mouthShape, fallbackEyeState.get, tachiePresets)
          case (Some(_), _) => getTachie(fallbackMouthShape.get, eyeState, tachiePresets)
          case (_, _) => ???
        }
      }
    }
  }
}

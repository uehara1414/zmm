package com.github.windymelt.zmm.domain.model.speech

import io.circe.{Decoder, Encoder, HCursor, Json}
import io.circe.parser._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

case class Mora(
                 text: String,
                 consonant: Option[String],
                 consonant_length: Option[Double],
                 vowel: String,
                 vowel_length: Double,
                 pitch: Double
               )

case class AccentPhrase(
                         moras: Seq[Mora],
                         accent: Int,
                         pause_mora: Option[Mora],
                         is_interrogative: Boolean
                       )

case class SpeechParameters(
                             accent_phrases: Seq[AccentPhrase],
                             speedScale: Double,
                             pitchScale: Double,
                             intonationScale: Double,
                             volumeScale: Double,
                             prePhonemeLength: Double,
                             postPhonemeLength: Double,
                             outputSamplingRate: Int,
                             outputStereo: Boolean,
                             kana: String
                           )


object AudioQueryParser {
  def parseJson(jsonString: String): Option[SpeechParameters] = {
    val parseResult = parse(jsonString)

    parseResult match {
      case Left(failure) => None
      case Right(json) =>
        val cursor: HCursor = json.hcursor

        val accentPhrases =
          cursor
            .downField("accent_phrases")
            .as[Seq[Json]]
        val speedScale = cursor.downField("speedScale").as[Double]
        val pitchScale = cursor.downField("pitchScale").as[Double]
        val intonationScale = cursor.downField("intonationScale").as[Double]
        val volumeScale = cursor.downField("volumeScale").as[Double]
        val prePhonemeLength = cursor.downField("prePhonemeLength").as[Double]
        val postPhonemeLength = cursor.downField("postPhonemeLength").as[Double]
        val outputSamplingRate = cursor.downField("outputSamplingRate").as[Int]
        val outputStereo = cursor.downField("outputStereo").as[Boolean]
        val kana = cursor.downField("kana").as[String]

        val accentPhraseList = accentPhrases match {
          case Right(phrases) =>
            phrases.map { phraseJson =>
              val moras = phraseJson.hcursor.downField("moras").as[Seq[Json]]
              val accent = phraseJson.hcursor.downField("accent").as[Int]
              val pauseMoraJson =
                phraseJson.hcursor.downField("pause_mora").as[Json]
              val isInterrogative =
                phraseJson.hcursor.downField("is_interrogative").as[Boolean]

              val pauseMora = pauseMoraJson match {
                case Right(moraJson) =>
                  val text = moraJson.hcursor.downField("text").as[String]
                  val consonant =
                    moraJson.hcursor.downField("consonant").as[Option[String]]
                  val consonantLength = moraJson.hcursor
                    .downField("consonant_length")
                    .as[Option[Double]]
                  val vowel = moraJson.hcursor.downField("vowel").as[String]
                  val vowelLength =
                    moraJson.hcursor.downField("vowel_length").as[Double]
                  val pitch = moraJson.hcursor.downField("pitch").as[Double]

                  Some(
                    Mora(
                      text.getOrElse(""),
                      consonant.getOrElse(null),
                      consonantLength.getOrElse(null),
                      vowel.getOrElse("a"),
                      vowelLength.getOrElse(0.05),
                      pitch.getOrElse(5.5)
                    )
                  )
                case Left(error) => Option.empty[Mora]
              }

              val moraList = moras match {
                case Right(moras) =>
                  moras.map { moraJson =>
                    val text = moraJson.hcursor.downField("text").as[String]
                    val consonant =
                      moraJson.hcursor.downField("consonant").as[Option[String]]
                    val consonantLength = moraJson.hcursor
                      .downField("consonant_length")
                      .as[Option[Double]]
                    val vowel = moraJson.hcursor.downField("vowel").as[String]
                    val vowelLength =
                      moraJson.hcursor.downField("vowel_length").as[Double]
                    val pitch = moraJson.hcursor.downField("pitch").as[Double]

                    // elseの値は適当。基本問題なく取れんじゃない？
                    Mora(
                      text.getOrElse(""),
                      consonant.getOrElse(null),
                      consonantLength.getOrElse(null),
                      vowel.getOrElse("a"),
                      vowelLength.getOrElse(0.05),
                      pitch.getOrElse(5.5)
                    )
                  }
                case Left(error) => Seq()
              }

              AccentPhrase(
                moraList,
                accent.getOrElse(5),
                pauseMora,
                isInterrogative.getOrElse(false)
              )
            }
          case Left(error) => Seq()
        }

        // ほとんど使わない値なのでelseの値は適当なサンプルから引っ張ってきている
        Some(SpeechParameters(
          accentPhraseList,
          speedScale.getOrElse(1.0),
          pitchScale.getOrElse(0.0),
          intonationScale.getOrElse(1.0),
          volumeScale.getOrElse(1.0),
          prePhonemeLength.getOrElse(0.1),
          postPhonemeLength.getOrElse(0.1),
          outputSamplingRate.getOrElse(24000),
          outputStereo.getOrElse(false),
          kana.getOrElse("")
        ))
    }
  }
}


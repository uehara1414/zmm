package com.github.windymelt.zmm.application

import cats.effect.IO
import com.github.windymelt.zmm.{domain, infrastructure, util}

class VoiceVoxStatus extends domain.repository.VoiceVoxComponent
  with infrastructure.VoiceVoxComponent
  with util.UtilComponent {

  val voiceVoxUri =
    sys.env.get("VOICEVOX_URI") getOrElse config.getString("voicevox.apiUri")
  def voiceVox: VoiceVox = new ConcreteVoiceVox(voiceVoxUri)

  def showVoiceVoxSpeakers(): IO[Unit] = {
    import io.circe.JsonObject
    import com.mitchtalmadge.asciidata.table.ASCIITable
    for {
      speakers <- voiceVox.speakers()
      speakersTable <- IO.pure {
        val speakersArray = speakers.asArray.get.flatMap(_.asObject)
        val styleToSeq = (name: String) =>
          (id: String) => (styleName: String) => Seq(name, id, styleName)
        val speakerToSeq = (speaker: JsonObject) => {
          val styles = speaker("styles").get.asArray.get.flatMap(_.asObject)
          styles map (s =>
            styleToSeq(speaker("name").get.asString.get)(
              s("id").get.asNumber.get.toString
            )(s("name").get.asString.get)
            )
        }
        speakersArray.flatMap(speakerToSeq).map(_.toArray).toArray
      }
      _ <- IO.println(
        ASCIITable.fromData(
          Seq("voice", "voice ID", "style").toArray,
          speakersTable
        )
      )
    } yield ()
  }

}

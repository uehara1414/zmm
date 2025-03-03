package com.github.windymelt.zmm
package infrastructure

import com.github.windymelt.zmm.domain.model.speech.{
  SpeechParametersParser,
  SpeechParameters,
  Mora
}

import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration

trait VoiceVoxComponent {
  self: domain.repository.VoiceVoxComponent =>

  import cats.effect.IO
  import org.http4s.ember.client._
  import org.http4s.client._
  import org.http4s.Request
  import org.http4s.Method
  import org.http4s.Headers
  import org.http4s.Uri
  import io.circe._
  import io.circe.literal._
  import org.http4s.circe.CirceEntityDecoder._

  type SpeakerInfo = Json // TODO: 必要に応じて高級なcase class / HListにする
  def voiceVox: VoiceVox

  /** VOICEVOX client.
    *
    * You can start local VOICEVOX container with Docker:
    * {{{
    * docker run --rm -it -p '127.0.0.1:50021:50021' voicevox/voicevox_engine:cpu-ubuntu20.04-latest
    * }}}
    */
  class ConcreteVoiceVox(voiceVoxUri: String) extends VoiceVox {
    def speakers(): IO[SpeakerInfo] = client.use { c =>
      val req =
        Request[IO](uri =
          Uri.fromString(s"${voiceVoxUri}/speakers").fold(throw _, identity)
        )
      c.expect[SpeakerInfo](req)
    }
    // TODO: localhost:50021決め打ちをやめる
    def audioQuery(text: String, speaker: String): IO[SpeechParameters] =
      client.use { c =>
        val uri = Uri
          .fromString(s"${voiceVoxUri}/audio_query")
          .map(
            _.copy(query =
              org.http4s.Query
                .fromMap(Map("speaker" -> Seq(speaker), "text" -> Seq(text)))
            )
          )
        val req = Request[IO](
          Method.POST,
          uri = uri.fold(throw _, identity),
          headers = Headers("accept" -> "application/json")
        )
        c.expect[Json](req)
          .flatMap(json =>
            IO.pure(SpeechParametersParser.parseJson(json.toString).get)
          )
      }

    def synthesis(
        speech: SpeechParameters,
        speaker: String
    ): IO[fs2.Stream[IO, Byte]] =
      client.use { c =>
        import io.circe.generic.auto._
        import io.circe.syntax._

        val query = org.http4s.Query.fromMap(Map("speaker" -> Seq(speaker)))
        val url = s"${voiceVoxUri}/synthesis"
        val uri = Uri.fromString(url).map(_.copy(query = query))

        val req = Request[IO](
          Method.POST,
          uri = uri.fold(throw _, identity),
          headers = Headers("Content-Type" -> "application/json"),
          body = fs2.Stream.fromIterator[IO](
            speech.asJson.toString().getBytes().iterator,
            64
          ) // TODO: chinksize適当に指定しているのでなんとかする
        )
        IO.pure(c.stream(req).flatMap(_.body))
      }

    def controlSpeed(
        speech: SpeechParameters,
        speed: String
    ): IO[SpeechParameters] = {
      IO.pure { speech.copy(speedScale = speed.toDouble) }
    }

    def registerDict(
        word: String,
        pronounce: String,
        lowerPoint: Int
    ): IO[Unit] = client.use { c =>
      val uri = Uri
        .fromString(s"${voiceVoxUri}/user_dict_word")
        .map(
          _.copy(
            query = org.http4s.Query.fromMap(
              Map(
                "surface" -> Seq(word),
                "pronunciation" -> Seq(pronounce),
                "accent_type" -> Seq(lowerPoint.toString)
              )
            )
          )
        )

      val req = Request[IO](
        Method.POST,
        uri = uri.fold(throw _, identity),
        headers = Headers("Content-Type" -> "application/json")
      )

      c.successful(req) *> IO.unit
    }

    def getVowels(speech: SpeechParameters): IO[Seq[Mora]] =
      IO.pure { speech.durationAdjustedMoras }

    private lazy val client = {
      import concurrent.duration._
      import scala.language.postfixOps
      EmberClientBuilder
        .default[IO]
        .withTimeout(5 minutes)
        .withIdleConnectionTime(10 minutes)
        .build
    }
  }
}

package com.github.windymelt.zmm
package infrastructure

import cats.effect.IO
import io.circe.{Decoder, Json}
import io.circe.generic.semiauto.deriveDecoder
import org.http4s.{EntityDecoder, Request, Uri}
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.circe.CirceEntityDecoder.*

case class SpeakerResponse(speakerName: String)

case class Speaker(
  name: String
)

trait CoeiroInkComponent {
  self: domain.repository.CoeiroInkComponent =>
  type Speakers = Json

  def coeiroInk: CoeiroInk

  class ConcreteCoeiroInk(var uri: String) extends CoeiroInk:
    implicit def speakerDecoder: Decoder[List[SpeakerResponse]] = deriveDecoder[List[SpeakerResponse]]
    implicit def responseDecoder:  EntityDecoder[IO, List[SpeakerResponse]] = circeEntityDecoder[IO, List[SpeakerResponse]]

    def speakers: IO[Speakers] = {
      client.use { c =>
        val url = s"$uri/v1/speakers"
        val req = Request[IO](uri = Uri.fromString(url).fold(throw _, identity))
        val res = c.expect[Json](req)

        for {
          _ <- IO.println(url)
          _ <- IO.println(res)
          r <- res
        } yield {
          r
        }
      }
    }

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

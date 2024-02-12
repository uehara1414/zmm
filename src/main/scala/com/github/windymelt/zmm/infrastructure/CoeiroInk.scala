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
  type Speakers = Seq[Speaker]

  def coeiroInk: CoeiroInk

  class ConcreteCoeiroInk(var uri: String) extends CoeiroInk:
    def speakers: IO[Speakers] = {
      client.use { c =>
        val url = s"$uri/v1/speakers"
        val req = Request[IO](uri = Uri.fromString(url).fold(throw _, identity))
        val res = c.expect[Json](req)
        val response = res.map(_.asArray.getOrElse(Vector()))

        for {
          result <- response.flatMap {
            vec => {
              IO {
                vec.map {
                  json =>
                    json
                      .asObject
                      .flatMap(_("speakerName"))
                      .flatMap(_.asString)}
              }
            }
          }
        } yield result.flatMap(_.toSeq).map(Speaker(_))
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

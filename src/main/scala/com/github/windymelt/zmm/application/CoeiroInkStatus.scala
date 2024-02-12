package com.github.windymelt.zmm.application

import cats.effect.IO
import com.github.windymelt.zmm.{domain, infrastructure, util}

class CoeiroInkStatus extends infrastructure.CoeiroInkComponent
  with domain.repository.CoeiroInkComponent
  with util.UtilComponent {
  override def coeiroInk: CoeiroInk = {
    val uri = sys.env.get("COEIROINK_URI") getOrElse config.getString("coeiroink.apiUri")
    new ConcreteCoeiroInk(uri)
  }

  def show(): IO[Unit] = {
    val speakerObject = coeiroInk.speakers
    val speakers = speakerObject.map(_.asArray.getOrElse(Vector()))
    for {
      result <- speakers.flatMap {
        vec => {
          IO {
            vec.map { json =>
              json.asObject.flatMap(_("speakerName"))
            }
          }
        }
      }
      _ <- IO.println(result.map(_.get))
    } yield ()
  }
}

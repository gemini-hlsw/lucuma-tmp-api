// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.`enum`.{MagnitudeBand, MagnitudeSystem}
import lucuma.core.math.MagnitudeValue
import lucuma.core.model.Magnitude
import cats.syntax.all._
import io.circe.Decoder
import io.circe.generic.semiauto._

object MagnitudeModel {

  final case class Input(
    value:  BigDecimal,
    band:   MagnitudeBand,
    system: Option[MagnitudeSystem],
    error:  Option[BigDecimal]
  ) {

    private def toMagnitudeValue(field: String, d: BigDecimal): ValidatedInput[MagnitudeValue] =
      MagnitudeValue
        .fromBigDecimal
        .getOption(d)
        .toValidNec(InputError.fromMessage(s"Could not read '$field' field value $d as a magnitude value"))

    val toMagnitude: ValidatedInput[Magnitude] =
      (toMagnitudeValue("value", value),
       error.traverse(e => toMagnitudeValue("error", e))
      ).mapN { (v, e) =>
        Magnitude(v, band, e, system.getOrElse(MagnitudeSystem.Vega))
      }

  }

  object Input {

    implicit val DecoderInput: Decoder[Input] =
      deriveDecoder[Input]

  }

}

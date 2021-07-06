// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.math.Coordinates

import cats.Eq
import cats.syntax.apply._
import io.circe.Decoder
import io.circe.generic.semiauto._


object CoordinatesModel {

  final case class Input(
    ra: RightAscensionModel.Input,
    dec: DeclinationModel.Input
  ) {

    val toCoordinates: ValidatedInput[Coordinates] =
      (ra.toRightAscension,
       dec.toDeclination
      ).mapN { (ra, dec) => Coordinates(ra, dec) }

  }

  object Input {

    implicit val DecoderCoordinatesInput: Decoder[Input] =
      deriveDecoder[Input]

    implicit val EqInput: Eq[Input] =
      Eq.by(in => (
        in.ra,
        in.dec
      ))

  }

}

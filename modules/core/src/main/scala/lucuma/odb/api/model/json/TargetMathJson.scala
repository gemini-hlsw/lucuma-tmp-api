// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package json

import lucuma.core.math.{
  Declination,
  Coordinates,
  Epoch,
  Offset,
  ProperVelocity,
  RadialVelocity,
  RightAscension
}

import io.circe.Decoder

trait TargetMathJson {

  import format.target._

  implicit val DecoderDeclination: Decoder[Declination] =
    FormatDeclination.decoder

  implicit val DecoderEpoch: Decoder[Epoch] =
    FormatEpoch.decoder

  implicit val DecoderRightAscension: Decoder[RightAscension] =
    FormatRightAscension.decoder

  implicit val DecoderOffsetP: Decoder[Offset.P] =
    FormatOffsetP.decoder

  implicit val DecoderOffsetQ: Decoder[Offset.Q] =
    FormatOffsetQ.decoder

  implicit val DecoderProperVelocityRa: Decoder[ProperVelocity.RA] =
    FormatProperVelocityRa.decoder

  implicit val DecoderProperVelocityDec: Decoder[ProperVelocity.Dec] =
    FormatProperVelocityDec.decoder

  implicit val DecoderRadialVelocity: Decoder[RadialVelocity] =
    FormatRadialVelocity.decoder

  implicit val DecoderOffset: Decoder[Offset] =
    Decoder.forProduct2(
      "p",
      "q"
    )(Offset.apply)

  implicit val DecoderProperVelocity: Decoder[ProperVelocity] =
    Decoder.forProduct2(
      "ra",
      "dec"
    )(ProperVelocity.apply)

  implicit val DecoderCoordinates: Decoder[Coordinates] =
    Decoder.forProduct2(
      "ra",
      "dec"
    )(Coordinates.apply)
}

object targetmath extends TargetMathJson

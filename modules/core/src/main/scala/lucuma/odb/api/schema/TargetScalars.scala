// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.schema.syntax.scalar._

import lucuma.core.math.{
  Declination,
  Epoch,
  Offset,
  ProperVelocity,
  RadialVelocity,
  RightAscension
}

import sangria.schema.ScalarType

/**
 * Scalar "String" specializations.  These appear in the schema, giving a clue
 * that not just any `String` value will work and providing validation checks
 * on input.
 */
trait TargetScalars {

  implicit val HmsStringType: ScalarType[RightAscension] =
    ScalarType.fromScalarFormat(
      "HmsString",
      "Target right ascension coordinate",
      lucuma.odb.api.model.format.target.FormatRightAscension
    )

  implicit val DmsStringType: ScalarType[Declination] =
    ScalarType.fromScalarFormat(
      "DmsString",
      "Target declination coordinate",
      lucuma.odb.api.model.format.target.FormatDeclination
    )

  implicit val EpochStringType: ScalarType[Epoch] =
    ScalarType.fromScalarFormat(
      "EpochString",
      "Reference observation epoch",
      lucuma.odb.api.model.format.target.FormatEpoch
    )

  implicit val OffsetPStringType: ScalarType[Offset.P] =
    ScalarType.fromScalarFormat(
      "OffsetPString",
      "Offset in p as signed decimal arcseconds",
      lucuma.odb.api.model.format.target.FormatOffsetP
    )

  implicit val OffsetQStringType: ScalarType[Offset.Q] =
    ScalarType.fromScalarFormat(
      "OffsetQString",
      "Offset in q as signed decimal arcseconds",
      lucuma.odb.api.model.format.target.FormatOffsetQ
    )

  implicit val ProperVelocityRaStringType: ScalarType[ProperVelocity.RA] =
    ScalarType.fromScalarFormat(
      "RadialVelocityRaString",
      "ProperVelocity in RA as signed decimal milliarcseconds per year",
      lucuma.odb.api.model.format.target.FormatProperVelocityRa
    )

  implicit val ProperVelocityDecStringType: ScalarType[ProperVelocity.Dec] =
    ScalarType.fromScalarFormat(
      "RadialVelocityDecString",
      "ProperVelocity in Dec as signed decimal milliarcseconds per year",
      lucuma.odb.api.model.format.target.FormatProperVelocityDec
    )

  implicit val RadialVelocityType: ScalarType[RadialVelocity] =
    ScalarType.fromScalarFormat(
      "RadialVelocityString",
      "Radial velocity m/s",
      lucuma.odb.api.model.format.target.FormatRadialVelocity
    )

}
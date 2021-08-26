// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.schema.syntax.scalar._
import lucuma.core.math.{Declination, Epoch, RightAscension}
import lucuma.core.model.EphemerisKey
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

  implicit val EphemerisKeyType: ScalarType[EphemerisKey] =
    ScalarType.fromScalarFormat(
      "EphemerisKey",
      "Horizons ID",
      lucuma.odb.api.model.format.target.FormatEphemerisKey
    )

}
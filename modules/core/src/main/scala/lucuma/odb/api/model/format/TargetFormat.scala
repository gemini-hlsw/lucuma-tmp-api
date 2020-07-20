// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.format

import atto._
import atto.Atto._
import lucuma.core.optics.Format
import lucuma.core.math.{
  Angle,
  Declination,
  Epoch,
  Offset,
  ProperVelocity,
  RadialVelocity,
  RightAscension
}

trait TargetFormat {

  private val DecimalFormat: Format[String, BigDecimal] =
    Format(s => Atto.bigDecimal.parseOnly(s).option, _.toString)

  val FormatDeclination: ScalarFormat[Declination] =
    ScalarFormat(Declination.fromStringSignedDMS, "[+/-]DD:MM:SS.sss")

  val FormatEpoch: ScalarFormat[Epoch] =
    ScalarFormat(Epoch.fromString, "[JB]YYYY.YYY")

  val FormatRightAscension: ScalarFormat[RightAscension] =
    ScalarFormat(RightAscension.fromStringHMS, "HH:MM:SS.sss")

  val FormatOffsetP: ScalarFormat[Offset.P] =
    ScalarFormat(
      DecimalFormat.composeFormat(Offset.P.signedDecimalArcseconds.reverse.asFormat),
      "signed decimal arcseconds"
    )

  val FormatOffsetQ: ScalarFormat[Offset.Q] =
    ScalarFormat(
      DecimalFormat.composeFormat(Offset.Q.signedDecimalArcseconds.reverse.asFormat),
      "signed decimal arcseconds"
    )

  val FormatProperVelocityRa: ScalarFormat[ProperVelocity.RA] =
    ScalarFormat(
      DecimalFormat.composeFormat(ProperVelocity.RA.milliarcsecondsPerYear.reverse.asFormat),
      "signed decimal milliarcseconds per year (mas/y)"
    )

  val FormatProperVelocityDec: ScalarFormat[ProperVelocity.Dec] =
    ScalarFormat(
      DecimalFormat.composeFormat(ProperVelocity.Dec.milliarcsecondsPerYear.reverse.asFormat),
      "signed decimal milliarcseconds per year (mas/y)"
    )

  val FormatRadialVelocity: ScalarFormat[RadialVelocity] =
    ScalarFormat(
      DecimalFormat.composePrism(RadialVelocity.fromMetersPerSecond),
      "meters per second"
    )

  val FormatParallax: ScalarFormat[Angle] =
    ScalarFormat(
      DecimalFormat.composeFormat(Angle.signedDecimalMilliarcseconds.reverse.asFormat),
      "signed decimal mas"
    )

}

object target extends TargetFormat

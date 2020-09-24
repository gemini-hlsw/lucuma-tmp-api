// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.format

import lucuma.core.math.{
  Declination,
  Epoch,
  RightAscension
}

trait TargetFormat {

  val FormatDeclination: ScalarFormat[Declination] =
    ScalarFormat(Declination.fromStringSignedDMS, "[+/-]DD:MM:SS.sss")

  val FormatEpoch: ScalarFormat[Epoch] =
    ScalarFormat(Epoch.fromString, "[JB]YYYY.YYY")

  val FormatRightAscension: ScalarFormat[RightAscension] =
    ScalarFormat(RightAscension.fromStringHMS, "HH:MM:SS.sss")

}

object target extends TargetFormat

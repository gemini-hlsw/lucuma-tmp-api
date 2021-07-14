// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.format

import lucuma.core.math.{Declination, Epoch, RightAscension}
import lucuma.core.model.EphemerisKey
import lucuma.core.optics.Format

trait TargetFormat {

  val FormatDeclination: ScalarFormat[Declination] =
    ScalarFormat(Declination.fromStringSignedDMS, "[+/-]DD:MM:SS.sss")

  val FormatEpoch: ScalarFormat[Epoch] =
    ScalarFormat(Format.fromPrism(Epoch.fromString), "[JB]YYYY.YYY")

  val FormatRightAscension: ScalarFormat[RightAscension] =
    ScalarFormat(RightAscension.fromStringHMS, "HH:MM:SS.sss")

  val FormatEphemerisKey: ScalarFormat[EphemerisKey] =
    ScalarFormat(EphemerisKey.fromString, "[Comet|AsteroidNew|AsteroidOld|MajorBody]_des")

}

object target extends TargetFormat

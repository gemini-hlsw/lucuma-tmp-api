// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.gmos.longslit

import eu.timepit.refined.types.all
import eu.timepit.refined.types.all.PosDouble
import lucuma.core.`enum`.{GmosNorthFpu, GmosSouthFpu, GmosXBinning, ImageQuality}
import lucuma.core.model.SourceProfile

trait XBinCalculator[U] {

  def xBin(u: U, sourceProfile: SourceProfile, imageQuality: ImageQuality, sampling: PosDouble): GmosXBinning

}

object XBinCalculator {

  implicit val XBinCalculatorNorth: XBinCalculator[GmosNorthFpu] =
    (u: GmosNorthFpu, sourceProfile: SourceProfile, imageQuality: ImageQuality, sampling: all.PosDouble) =>
      GmosNorthLongslitMath.xbin(u, sourceProfile, imageQuality, sampling)

  implicit val XBinCalculatorSouth: XBinCalculator[GmosSouthFpu] =
    (u: GmosSouthFpu, sourceProfile: SourceProfile, imageQuality: ImageQuality, sampling: all.PosDouble) =>
      GmosSouthLongslitMath.xbin(u, sourceProfile, imageQuality, sampling)

}

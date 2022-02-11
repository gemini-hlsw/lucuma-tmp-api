// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen.gmos.longslit.syntax

import lucuma.core.`enum`.{GmosNorthFpu, GmosXBinning, ImageQuality}
import lucuma.core.model.SourceProfile

final class GmosNorthFpuOps(val self: GmosNorthFpu) extends AnyVal {

  def xbin(p: SourceProfile, iq: ImageQuality, sampling: Double): GmosXBinning =
    GmosNorthLongslitMath.xbin(self, p, iq, sampling)

}

trait ToGmosNorthFpuOps {
  implicit def toGmosNorthFpuOps(fpu: GmosNorthFpu): GmosNorthFpuOps =
    new GmosNorthFpuOps(fpu)
}

object gmosNorthFpu extends ToGmosNorthFpuOps

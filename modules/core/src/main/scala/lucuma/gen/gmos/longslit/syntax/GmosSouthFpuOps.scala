// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen.gmos.longslit.syntax

import lucuma.core.`enum`.{GmosSouthFpu, GmosXBinning, ImageQuality}
import lucuma.core.model.SourceProfile

final class GmosSouthFpuOps(val self: GmosSouthFpu) extends AnyVal {

  def xbin(p: SourceProfile, iq: ImageQuality, sampling: Double): GmosXBinning =
    GmosSouthLongslitMath.xbin(self, p, iq, sampling)

}

trait ToGmosSouthFpuOps {
  implicit def toGmosSouthFpuOps(fpu: GmosSouthFpu): GmosSouthFpuOps =
    new GmosSouthFpuOps(fpu)
}

object gmosSouthFpu extends ToGmosSouthFpuOps

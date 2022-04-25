// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen.gmos.longslit.syntax

import coulomb.Quantity
import lucuma.core.`enum`.GmosSouthGrating
import lucuma.core.math.units.Nanometer

final class GmosSouthGratingOps(val self: GmosSouthGrating) {

  val Δλ: Quantity[Int, Nanometer] =
    GmosSouthLongslitMath.Δλ(self.dispersion)

}

trait ToGmosSouthGratingOps {
  implicit def toGmosSouthGratingOps(grating: GmosSouthGrating): GmosSouthGratingOps =
    new GmosSouthGratingOps(grating)
}

object gmosSouthGrating extends ToGmosSouthGratingOps

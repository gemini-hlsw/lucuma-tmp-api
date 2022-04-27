// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen.gmos.longslit.syntax

import coulomb.Quantity
import eu.timepit.refined.types.all.PosInt
import lucuma.core.`enum`.GmosNorthGrating
import lucuma.core.math.units.Nanometer

final class GmosNorthGratingOps(val self: GmosNorthGrating) {

  val Δλ: Quantity[PosInt, Nanometer] =
    GmosNorthLongslitMath.Δλ(self.dispersion)

}

trait ToGmosNorthGratingOps {
  implicit def toGmosNorthGratingOps(grating: GmosNorthGrating): GmosNorthGratingOps =
    new GmosNorthGratingOps(grating)
}

object gmosNorthGrating extends ToGmosNorthGratingOps

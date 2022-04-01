// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen.gmos.longslit.syntax

import coulomb.Quantity
import eu.timepit.refined.types.all.PosInt
import lucuma.core.`enum`.GmosSouthDisperser
import lucuma.core.math.units.Nanometer

final class GmosSouthDisperserOps(val self: GmosSouthDisperser) {

  val Δλ: Quantity[PosInt, Nanometer] =
    GmosSouthLongslitMath.Δλ(self.dispersion)

}

trait ToGmosSouthDisperserOps {
  implicit def toGmosSouthDisperserOps(disperser: GmosSouthDisperser): GmosSouthDisperserOps =
    new GmosSouthDisperserOps(disperser)
}

object gmosSouthDisperser extends ToGmosSouthDisperserOps

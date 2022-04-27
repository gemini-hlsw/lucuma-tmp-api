// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.gmos.longslit

import coulomb.Quantity
import lucuma.core.`enum`.{GmosNorthDisperser, GmosSouthDisperser}
import lucuma.core.math.units.Nanometer

trait DeltaWavelengthCalculator[G] {

  def Δλ(g: G): Quantity[Int, Nanometer]

}

object DeltaWavelengthCalculator {

  implicit val DeltaWavelengthCalculatorGmosNorthGrating: DeltaWavelengthCalculator[GmosNorthDisperser] =
    (g: GmosNorthDisperser) => GmosNorthLongslitMath.Δλ(g.dispersion)

  implicit val DeltaWavelengthCalculatorGmosSouthGrating: DeltaWavelengthCalculator[GmosSouthDisperser] =
    (g: GmosSouthDisperser) => GmosSouthLongslitMath.Δλ(g.dispersion)

}

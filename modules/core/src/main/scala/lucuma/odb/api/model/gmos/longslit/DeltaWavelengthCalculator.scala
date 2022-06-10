// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.gmos.longslit

import coulomb.Quantity
import lucuma.core.`enum`.{GmosNorthGrating, GmosSouthGrating}
import lucuma.core.math.units.Nanometer

trait DeltaWavelengthCalculator[G] {

  def Δλ(g: G): Quantity[BigDecimal, Nanometer]

}

object DeltaWavelengthCalculator {

  implicit val DeltaWavelengthCalculatorGmosNorthGrating: DeltaWavelengthCalculator[GmosNorthGrating] =
    (g: GmosNorthGrating) => GmosNorthLongslitMath.Δλ(g.dispersion)

  implicit val DeltaWavelengthCalculatorGmosSouthGrating: DeltaWavelengthCalculator[GmosSouthGrating] =
    (g: GmosSouthGrating) => GmosSouthLongslitMath.Δλ(g.dispersion)

}

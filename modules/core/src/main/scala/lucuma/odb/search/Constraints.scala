// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.search

import lucuma.core.math.Wavelength
import eu.timepit.refined.types.numeric.PosInt

/** Observing constraints, used to narrow the space of compatible observing modes. */
sealed trait Constraints

object Constraints {

  /** Observing constraints for spectroscopy. */
  final case class Spectroscopy(
    λ:                    Wavelength,
    simultaneousCoverage: Wavelength,
    resolution:           PosInt, // todo: Resolution
  ) extends Constraints

}

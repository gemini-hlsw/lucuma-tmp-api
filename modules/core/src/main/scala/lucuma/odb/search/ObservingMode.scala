// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.search

import lucuma.core.enum._
import lucuma.core.math.Wavelength
import lucuma.odb.search.syntax.gmosnorthdisperser._
import lucuma.odb.search.syntax.gmosnorthfpu._
import lucuma.odb.search.syntax.gmosnorthfilter._

sealed trait ObservingMode {
  def instrument: Instrument
}

object ObservingMode {

  sealed trait Spectroscopy extends ObservingMode {
    def λ:          Wavelength
    def resolution: Int
    def coverage:   Coverage
  }

  object Spectroscopy {

    final case class GmosNorth(
      λ:         Wavelength,
      disperser: GmosNorthDisperser,
      fpu:       GmosNorthFpu,
      filter:    Option[GmosNorthFilter]
    ) extends Spectroscopy {

      val instrument: Instrument =
        Instrument.GmosNorth

      def resolution: Int =
        disperser.resolution(λ, fpu.effectiveSlitWidth)

      def coverage: Coverage =
        filter.foldLeft(disperser.coverage(λ))(_ ⋂ _.coverage)

    }

  }

}

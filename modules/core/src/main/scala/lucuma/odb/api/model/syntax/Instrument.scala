// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.syntax

import lucuma.core.enum.Instrument
import scala.concurrent.duration._

final class InstrumentOps(val self: Instrument) extends AnyVal {

  /** Minimum exposure time allowed by the instrument. Always positive. */
  def minExposureDuration: FiniteDuration =
    self match {
      case Instrument.GmosNorth => 1.second
      case Instrument.GmosSouth => 1.second
      case i                => sys.error(s"Minimum exposure time for $i is not know.")
    }

  /** Minimum exposure time allowed by the instrument. Always greater than `minExposureDuration`. */
  def maxExposureDuration: FiniteDuration =
    self match {
      case Instrument.GmosNorth => 20.minutes
      case Instrument.GmosSouth => 20.minutes
      case i                => sys.error(s"Maximum exposure time for $i is not know.")
    }

  /** True if the instrument requires exposure times in integral seconds. */
  def integralDurations: Boolean =
    self match {
      case Instrument.GmosNorth => true
      case Instrument.GmosSouth => true
      case i                => sys.error(s"Integral durations for $i is not know.")
    }

}

trait ToInstrumentOps {
  implicit def toInstrumentOps(self: Instrument): InstrumentOps =
    new InstrumentOps(self)
}

object instrument extends ToInstrumentOps

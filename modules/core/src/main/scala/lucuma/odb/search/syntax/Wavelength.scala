// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.search.syntax

import coulomb.refined._
import lucuma.core.math.Wavelength
import spire.std.int._

final class WavelengthOps(val self: Wavelength) extends AnyVal {

  /** Returns the difference of this wavelength and `other`, clipped at Wavelength.Min. */
  def -(other: Wavelength): Wavelength =
    Wavelength.fromPicometers.getOption(self.toPicometers.value.value - other.toPicometers.value.value)
      .getOrElse(Wavelength.Min)

  /** Returns the sum of this wavelength and `other`, clipped at Wavelength.Max. */
  def +(other: Wavelength): Wavelength =
    Some(self.toPicometers + other.toPicometers)
      .filter(_ <= Wavelength.Max.toPicometers)
      .map(Wavelength.apply)
      .getOrElse(Wavelength.Max)

}

trait ToWavelengthOps {
  implicit def toWavelengthOps(self: Wavelength): WavelengthOps =
    new WavelengthOps(self)
}

object wavelength extends ToWavelengthOps

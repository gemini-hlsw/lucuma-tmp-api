// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import WavelengthModel.WavelengthInput
import eu.timepit.refined.types.all.{PosBigDecimal, PosInt}
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbRefined
import org.scalacheck._

trait ArbWavelengthModel {

  import ArbRefined._

  private def genBigDecimal(max: Int): Gen[PosBigDecimal] =
    Gen.chooseNum(1,  max).map(PosBigDecimal.unsafeFrom(_))

  private[this] val picometers: Gen[PosInt]        = Gen.chooseNum(1, Wavelength.Max.toPicometers.value.value).map(PosInt.unsafeFrom)
  private[this] val angstroms: Gen[PosBigDecimal]   = genBigDecimal(Wavelength.MaxAngstrom)
  private[this] val nanometers: Gen[PosBigDecimal]  = genBigDecimal(Wavelength.MaxNanometer)
  private[this] val micrometers: Gen[PosBigDecimal] = genBigDecimal(Wavelength.MaxMicrometer)

  implicit val arbWavelengthModelInput: Arbitrary[WavelengthModel.WavelengthInput] =
    Arbitrary {
      Gen.oneOf(
        picometers.map(WavelengthInput.fromPicometers),
        angstroms.map(WavelengthInput.fromAngstroms),
        nanometers.map(WavelengthInput.fromNanometers),
        micrometers.map(WavelengthInput.fromMicrometers)
      )
    }

  implicit val cogWavelengthModelInput: Cogen[WavelengthModel.WavelengthInput] =
    Cogen[(
      Option[PosInt],        // pm
      Option[PosBigDecimal], // A
      Option[PosBigDecimal], // nm
      Option[PosBigDecimal]  // µm
    )].contramap { in =>
      (
        in.picometers,
        in.angstroms,
        in.nanometers,
        in.micrometers
      )
    }

}

object ArbWavelengthModel extends ArbWavelengthModel



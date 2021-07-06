// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import WavelengthModel.{Input, Units}
import NumericUnits.{DecimalInput, LongInput}
import lucuma.core.math.Wavelength
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck._

trait ArbWavelengthModel {

  import ArbEnumerated._
  import GenNumericUnitsInput._

  private def genBigDecimal(max: Int): Gen[BigDecimal] =
    Gen.chooseNum(1,  max).map(BigDecimal(_))

  private[this] val picometers: Gen[Long]        = Gen.chooseNum(1, Wavelength.Max.toPicometers.value.value).map(_.toLong)
  private[this] val angstroms: Gen[BigDecimal]   = genBigDecimal(Wavelength.MaxAngstrom)
  private[this] val nanometers: Gen[BigDecimal]  = genBigDecimal(Wavelength.MaxNanometer)
  private[this] val micrometers: Gen[BigDecimal] = genBigDecimal(Wavelength.MaxMicrometer)

  val genWavelengthModelInputFromLong: Gen[Input] =
    Gen.oneOf(
      genLongInput(picometers,  Units.picometers),
      genLongInput(angstroms,   Units.angstroms),
      genLongInput(nanometers,  Units.nanometers),
      genLongInput(micrometers, Units.micrometers)
    ).map(Input.fromLong)

  val genWavelengthModelInputFromDecimal: Gen[Input] =
    Gen.oneOf(
      genLongDecimalInput(picometers,  Units.picometers),
      genDecimalInput(angstroms,   Units.angstroms),
      genDecimalInput(nanometers,  Units.nanometers),
      genDecimalInput(micrometers, Units.micrometers)
    ).map(Input.fromDecimal)

  implicit val arbWavelengthModelInput: Arbitrary[WavelengthModel.Input] =
    Arbitrary {
      Gen.oneOf(
        picometers.map(Input.fromPicometers),
        angstroms.map(Input.fromAngstroms),
        nanometers.map(Input.fromNanometers),
        micrometers.map(Input.fromMicrometers),
        genWavelengthModelInputFromLong,
        genWavelengthModelInputFromDecimal
      )
    }

  implicit val cogWavelengthModelInput: Cogen[WavelengthModel.Input] =
    Cogen[(
      Option[Long],       // pm
      Option[BigDecimal], // A
      Option[BigDecimal], // nm
      Option[BigDecimal], // µm
      Option[LongInput[Units]],
      Option[DecimalInput[Units]]
    )].contramap { in =>
      (
        in.picometers,
        in.angstroms,
        in.nanometers,
        in.micrometers,
        in.fromLong,
        in.fromDecimal
      )
    }

}

object ArbWavelengthModel extends ArbWavelengthModel



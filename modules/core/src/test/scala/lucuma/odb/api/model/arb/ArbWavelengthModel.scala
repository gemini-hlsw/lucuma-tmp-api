// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import WavelengthModel.{Input, Units}
import NumericUnits.{LongInput, DecimalInput}

import lucuma.core.util.arb.ArbEnumerated

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary


trait ArbWavelengthModel {

  import ArbEnumerated._
  import GenNumericUnitsInput._

  private[this] val picometers: Gen[Long] =
    arbitrary[Int].map(_.toLong)

  private[this] val angstroms: Gen[BigDecimal] =
    picometers.map(n => BigDecimal(n)/100)

  private[this] val nanometers: Gen[BigDecimal] =
    angstroms.map(_/10)

  private[this] val micrometers: Gen[BigDecimal] =
    nanometers.map(_/1000)

  val genWavelengthModelInputFromLong: Gen[Input] =
    Gen.oneOf(
      genLongInput(picometers,  Units.picometers),
      genLongInput(angstroms,   Units.angstroms),
      genLongInput(nanometers,  Units.nanometers),
      genLongInput(micrometers, Units.micrometers)
    ).map(Input.fromLong)

  val genWavelengthModelInputFromDecimal: Gen[Input] =
    Gen.oneOf(
      genDecimalInput(picometers,  Units.picometers),
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



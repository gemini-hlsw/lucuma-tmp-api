// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import DeclinationModel.{Input, Units}
import NumericUnits.{LongInput, DecimalInput}

import lucuma.core.math.Declination
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.math.arb.ArbDeclination

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbDeclinationModel {
  import ArbEnumerated._
  import ArbDeclination._
  import GenNumericUnitsInput._

  private[this] val microarcseconds: Gen[Long] =
    arbitrary[Declination].map(_.toAngle.toMicroarcseconds)

  private[this] val degrees: Gen[BigDecimal] =
    arbitrary[Declination].map(d => BigDecimal(d.toAngle.toDoubleDegrees))

  val genDeclinationModelInputFromLong: Gen[Input] =
    Gen.oneOf(
      genLongInput(microarcseconds, Units.microarcseconds),
      genLongInput(degrees, Units.degrees)
    ).map(Input.fromLong)

  val genDeclinationModelInputFromDecimal: Gen[Input] =
    Gen.oneOf(
      genLongDecimalInput(microarcseconds, Units.microarcseconds),
      genDecimalInput(degrees, Units.degrees)
    ).map(Input.fromDecimal)

  implicit val arbDeclinationModelInput: Arbitrary[DeclinationModel.Input] =
    Arbitrary {
      Gen.oneOf(
        microarcseconds.map(Input.fromMicroarcseconds),
        degrees.map(Input.fromDegrees),
        arbitrary[Declination].map(Input.fromDms),
        genDeclinationModelInputFromLong,
        genDeclinationModelInputFromDecimal
      )
    }

  implicit val cogDeclinationModelInput: Cogen[DeclinationModel.Input] =
    Cogen[(
      Option[Long],
      Option[BigDecimal],
      Option[Declination],
      Option[LongInput[Units]],
      Option[DecimalInput[Units]]
    )].contramap { in =>
      (in.microarcseconds, in.degrees, in.dms, in.fromLong, in.fromDecimal)
    }
}

object ArbDeclinationModel extends ArbDeclinationModel


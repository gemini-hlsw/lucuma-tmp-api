// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import NumericUnits.{DecimalInput, LongInput}
import RadialVelocityModel.{Input, Units}

import lucuma.core.math.RadialVelocity
import lucuma.core.math.arb.ArbRadialVelocity
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary


trait ArbRadialVelocityModel {

  import ArbEnumerated._
  import ArbRadialVelocity._
  import GenNumericUnitsInput._

  private[this] val cmPerSec: Gen[Long] =
    arbitrary[RadialVelocity].map(_.rv.value.underlying.movePointRight(3).longValue)

  private[this] val mPerSec: Gen[BigDecimal] =
    arbitrary[RadialVelocity].map(_.rv.value)

  private[this] val kmPerSec: Gen[BigDecimal] =
    arbitrary[RadialVelocity].map(RadialVelocity.kilometerspersecond.reverseGet)

  val genRadialVelocityModelInputFromLong: Gen[Input] =
    Gen.oneOf(
      genLongInput(cmPerSec, Units.centimetersPerSecond),
      genLongInput(mPerSec, Units.metersPerSecond),
      genLongInput(kmPerSec, Units.kilometersPerSecond)
    ).map(Input.fromLong)

  val genRadialVelocityModelInputFromDecimal: Gen[Input] =
    Gen.oneOf(
      genLongDecimalInput(cmPerSec, Units.centimetersPerSecond),
      genDecimalInput(mPerSec, Units.metersPerSecond),
      genDecimalInput(kmPerSec, Units.kilometersPerSecond)
    ).map(Input.fromDecimal)

  implicit val arbRadialVelocityModelInput: Arbitrary[RadialVelocityModel.Input] =
    Arbitrary {
      Gen.oneOf(
        cmPerSec.map(Input.fromCentimetersPerSecond),
        mPerSec.map(Input.fromMetersPerSecond),
        kmPerSec.map(Input.fromKilometersPerSecond),
        genRadialVelocityModelInputFromLong,
        genRadialVelocityModelInputFromDecimal
      )
    }

  implicit val cogRadialVelocityModelInput: Cogen[RadialVelocityModel.Input] =
    Cogen[(
      Option[Long],
      Option[BigDecimal],
      Option[BigDecimal],
      Option[LongInput[Units]],
      Option[DecimalInput[Units]]
    )].contramap { in =>
      (in.centimetersPerSecond, in.metersPerSecond, in.kilometersPerSecond, in.fromLong, in.fromDecimal)
    }
}

object ArbRadialVelocityModel extends ArbRadialVelocityModel

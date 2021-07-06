// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import NumericUnits.{DecimalInput, LongInput}
import ProperMotionModel.{ComponentInput, Input, Units}
import lucuma.core.math.ProperMotion.AngularVelocityComponent
import lucuma.core.math.arb.ArbProperMotion
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

import java.math.MathContext


trait ArbProperMotionModel {

  import ArbEnumerated._
  import ArbProperMotion._
  import GenNumericUnitsInput._

  private[this] def microarcsecondsPerYear: Gen[Long] =
    arbitrary[AngularVelocityComponent[Unit]].map(_.μasy.value)

  private[this] def milliArcSecondPerYear: Gen[BigDecimal] =
    arbitrary[AngularVelocityComponent[Unit]].map(_.masy.value.toBigDecimal(MathContext.UNLIMITED))

  val genProperMotionModelComponentInputFromLong: Gen[ComponentInput] =
    Gen.oneOf(
      genLongInput(microarcsecondsPerYear, Units.microarcsecondsPerYear),
      genLongInput(milliArcSecondPerYear, Units.milliarcsecondsPerYear)
    ).map(ComponentInput.fromLong)

  val genProperMotionModelComponentInputFromDecimal: Gen[ComponentInput] =
    Gen.oneOf(
      genLongDecimalInput(microarcsecondsPerYear, Units.microarcsecondsPerYear),
      genDecimalInput(milliArcSecondPerYear, Units.milliarcsecondsPerYear)
    ).map(ComponentInput.fromDecimal)

  implicit val arbProperMotionModelComponentInput: Arbitrary[ComponentInput] =
    Arbitrary {
      Gen.oneOf(
        microarcsecondsPerYear.map(ComponentInput.fromMicroarcsecondsPerYear),
        milliArcSecondPerYear.map(ComponentInput.fromMilliarcsecondsPerYear),
        genProperMotionModelComponentInputFromLong,
        genProperMotionModelComponentInputFromDecimal
      )
    }

  implicit val cogProperMotionModelComponentInput: Cogen[ComponentInput] =
    Cogen[(
      Option[Long],
      Option[BigDecimal],
      Option[LongInput[Units]],
      Option[DecimalInput[Units]]
    )].contramap { in =>
      (in.microarcsecondsPerYear, in.milliarcsecondsPerYear, in.fromLong, in.fromDecimal)
    }

  implicit val arbProperMotionModelInput: Arbitrary[Input] =
    Arbitrary {
      for {
        r <- arbitrary[ComponentInput]
        d <- arbitrary[ComponentInput]
      } yield Input(r, d)
    }

  implicit val cogProperMotionModelInput: Cogen[Input] =
    Cogen[(ComponentInput, ComponentInput)].contramap { in =>
      (in.ra, in.dec)
    }
}

object ArbProperMotionModel extends ArbProperMotionModel

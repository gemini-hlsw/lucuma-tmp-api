// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import RightAscensionModel.{Input, Units}
import lucuma.core.math.RightAscension
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.math.arb.ArbRightAscension
import lucuma.odb.api.model.NumericUnits.{DecimalInput, LongInput}
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbRightAscensionModel {
  import ArbEnumerated._
  import ArbRightAscension._
  import GenNumericUnitsInput._

  private[this] val microarcseconds: Gen[Long] =
    arbitrary[RightAscension].map(_.toAngle.toMicroarcseconds)

  private[this] val degrees: Gen[BigDecimal] =
    arbitrary[RightAscension].map(_.toAngle.toDoubleDegrees)

  private[this] val hours: Gen[BigDecimal] =
    arbitrary[RightAscension].map(_.toHourAngle.toDoubleHours)

  val genRightAscensionModelInputFromLong: Gen[Input] =
    Gen.oneOf(
      genLongInput(microarcseconds, Units.microarcseconds),
      genLongInput(degrees, Units.degrees),
      genLongInput(hours, Units.hours)
    ).map(Input.fromLong)

  val genRightAscensionModelInputFromDecimal: Gen[Input] =
    Gen.oneOf(
      genLongDecimalInput(microarcseconds, Units.microarcseconds),
      genDecimalInput(degrees, Units.degrees),
      genDecimalInput(hours, Units.hours)
    ).map(Input.fromDecimal)

  implicit val arbRightAscensionModelInput: Arbitrary[RightAscensionModel.Input] =
    Arbitrary {
      Gen.oneOf(
        microarcseconds.map(Input.fromMicroarcseconds),
        degrees.map(Input.fromDegrees),
        hours.map(Input.fromHours),
        arbitrary[RightAscension].map(Input.fromHms),
        genRightAscensionModelInputFromLong,
        genRightAscensionModelInputFromDecimal
      )
    }

  implicit val cogRightAscensionModelInput: Cogen[RightAscensionModel.Input] =
    Cogen[(
      Option[Long],
      Option[BigDecimal],
      Option[BigDecimal],
      Option[RightAscension],
      Option[LongInput[Units]],
      Option[DecimalInput[Units]]
    )].contramap { in =>
      (in.microarcseconds, in.degrees, in.hours, in.hms, in.fromLong, in.fromDecimal)
    }
}

object ArbRightAscensionModel extends ArbRightAscensionModel

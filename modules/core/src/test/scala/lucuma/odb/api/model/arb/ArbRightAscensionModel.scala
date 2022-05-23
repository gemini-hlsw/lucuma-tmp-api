// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import RightAscensionModel.Input
import lucuma.core.math.RightAscension
import lucuma.core.math.arb.ArbRightAscension
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbRightAscensionModel {
  import ArbRightAscension._

  private[this] val microarcseconds: Gen[Long] =
    arbitrary[RightAscension].map(_.toAngle.toMicroarcseconds)

  private[this] val degrees: Gen[BigDecimal] =
    arbitrary[RightAscension].map(_.toAngle.toDoubleDegrees)

  private[this] val hours: Gen[BigDecimal] =
    arbitrary[RightAscension].map(_.toHourAngle.toDoubleHours)

  implicit val arbRightAscensionModelInput: Arbitrary[RightAscensionModel.Input] =
    Arbitrary {
      Gen.oneOf(
        microarcseconds.map(Input.fromMicroarcseconds),
        degrees.map(Input.fromDegrees),
        hours.map(Input.fromHours),
        arbitrary[RightAscension].map(Input.fromHms)
      )
    }

  implicit val cogRightAscensionModelInput: Cogen[RightAscensionModel.Input] =
    Cogen[(
      Option[Long],
      Option[BigDecimal],
      Option[BigDecimal],
      Option[RightAscension]
    )].contramap { in =>
      (in.microarcseconds, in.degrees, in.hours, in.hms)
    }
}

object ArbRightAscensionModel extends ArbRightAscensionModel

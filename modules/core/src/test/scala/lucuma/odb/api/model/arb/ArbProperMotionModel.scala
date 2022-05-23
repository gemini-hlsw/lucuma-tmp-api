// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import ProperMotionModel.{ComponentInput, Input}
import lucuma.core.math.ProperMotion.AngularVelocityComponent
import lucuma.core.math.arb.ArbProperMotion
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

import java.math.MathContext


trait ArbProperMotionModel {

  import ArbProperMotion._

  private[this] def microarcsecondsPerYear: Gen[Long] =
    arbitrary[AngularVelocityComponent[Unit]].map(_.μasy.value)

  private[this] def milliArcSecondPerYear: Gen[BigDecimal] =
    arbitrary[AngularVelocityComponent[Unit]].map(_.masy.value.toBigDecimal(MathContext.UNLIMITED))

  implicit val arbProperMotionModelComponentInput: Arbitrary[ComponentInput] =
    Arbitrary {
      Gen.oneOf(
        microarcsecondsPerYear.map(ComponentInput.fromMicroarcsecondsPerYear),
        milliArcSecondPerYear.map(ComponentInput.fromMilliarcsecondsPerYear)
      )
    }

  implicit val cogProperMotionModelComponentInput: Cogen[ComponentInput] =
    Cogen[(
      Option[Long],
      Option[BigDecimal],
    )].contramap { in =>
      (in.microarcsecondsPerYear, in.milliarcsecondsPerYear)
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

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import clue.data.Input
import lucuma.core.math.arb.ArbAngle
import lucuma.core.math.Angle
import lucuma.odb.api.model.AngleModel.AngleInput
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary


trait ArbPosAngleConstraint {

  import ArbAngle._
  import ArbAngleModel._
  import ArbInput._

  implicit val arbFixedPosAngleConstraint: Arbitrary[PosAngleConstraint.Fixed] =
    Arbitrary {
      for {
        a <- arbitrary[Angle]
        f <- arbitrary[Boolean]
      } yield PosAngleConstraint.Fixed(a, f)
    }

  implicit val cogFixedPosAngleConstraint: Cogen[PosAngleConstraint.Fixed] =
    Cogen[(
      Angle,
      Boolean
    )].contramap { in => (
      in.angle,
      in.allowFlip
    )}

  implicit val arbAverageParallacticPosAngleConstraint: Arbitrary[PosAngleConstraint.AverageParallactic] =
    Arbitrary {
      arbitrary[Option[Angle]].map(a => PosAngleConstraint.AverageParallactic(a))
    }

  implicit val cogAverageParallacticPosAngleConstraint: Cogen[PosAngleConstraint.AverageParallactic] =
    Cogen[Option[Angle]].contramap(_.overrideAngle)

  implicit val arbPosAngleConstraint: Arbitrary[PosAngleConstraint] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[PosAngleConstraint.Fixed],
        arbitrary[PosAngleConstraint.AverageParallactic]
      )
    }

  implicit val cogPosAngleConstraint: Cogen[PosAngleConstraint] =
    Cogen[(
      Option[PosAngleConstraint.Fixed],
      Option[PosAngleConstraint.AverageParallactic]
    )].contramap { in => (
      PosAngleConstraint.fixed.getOption(in),
      PosAngleConstraint.averageParallactic.getOption(in)
    )}

  implicit val arbFixedPosAngleConstraintInput: Arbitrary[PosAngleConstraintInput.FixedInput] =
    Arbitrary {
      for {
        a <- arbitrary[Input[AngleInput]]
        f <- arbitrary[Input[Boolean]]
      } yield PosAngleConstraintInput.FixedInput(a, f)
    }

  implicit val cogFixedPosAngleConstraintInput: Cogen[PosAngleConstraintInput.FixedInput] =
    Cogen[(
      Input[AngleInput],
      Input[Boolean]
    )].contramap { in => (
      in.angle,
      in.allowFlip
    )}

  implicit val arbAverageParallacticPosAngleConstraintInput: Arbitrary[PosAngleConstraintInput.AverageParallacticInput] =
    Arbitrary {
      arbitrary[Input[AngleInput]].map(PosAngleConstraintInput.AverageParallacticInput(_))
    }

  implicit val cogAverageParallacticPosAngleConstraintInput: Cogen[PosAngleConstraintInput.AverageParallacticInput] =
    Cogen[Input[AngleInput]].contramap(_.overrideAngle)

  implicit val arbPosAngleConstraintInput: Arbitrary[PosAngleConstraintInput] =
    Arbitrary {
      for {
        f <- arbitrary[Input[PosAngleConstraintInput.FixedInput]]
        a <- arbitrary[Input[PosAngleConstraintInput.AverageParallacticInput]]
      } yield PosAngleConstraintInput(f, a)
    }

  implicit val cogPosAngleConstraintInput: Cogen[PosAngleConstraintInput] =
    Cogen[(
      Input[PosAngleConstraintInput.FixedInput],
      Input[PosAngleConstraintInput.AverageParallacticInput]
    )].contramap { in => (
      in.fixed,
      in.averageParallactic
    )}
}

object ArbPosAngleConstraint extends ArbPosAngleConstraint
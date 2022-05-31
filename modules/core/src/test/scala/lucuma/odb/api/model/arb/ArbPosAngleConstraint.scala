// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import clue.data.Input
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.math.arb.ArbAngle
import lucuma.core.math.Angle
import lucuma.odb.api.model.AngleModel.AngleInput
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary


trait ArbPosAngleConstraint {

  import ArbAngle._
  import ArbAngleModel._
  import ArbEnumerated._
  import ArbInput._

  implicit val arbPosAngleConstraint: Arbitrary[PosAngleConstraint] =
    Arbitrary {
      for {
        c <- arbitrary[PosAngleConstraint.Type]
        a <- arbitrary[Option[Angle]]
      } yield PosAngleConstraint(c, a)
    }

  implicit val cogPosAngleConstraint: Cogen[PosAngleConstraint] =
    Cogen[(
      PosAngleConstraint.Type,
      Option[Angle]
    )].contramap { in => (
      in.constraint,
      in.angle
    )}

  implicit val arbPosAngleConstraintInput: Arbitrary[PosAngleConstraintInput] =
    Arbitrary {
      for {
        c <- arbitrary[Input[PosAngleConstraint.Type]]
        a <- arbitrary[Input[AngleInput]]
      } yield PosAngleConstraintInput(c, a)
    }

  implicit val cogPosAngleConstraintInput: Cogen[PosAngleConstraintInput] =
    Cogen[(
      Input[PosAngleConstraint.Type],
      Input[AngleInput]
    )].contramap { in => (
      in.constraint,
      in.angle
    )}

}

object ArbPosAngleConstraint extends ArbPosAngleConstraint
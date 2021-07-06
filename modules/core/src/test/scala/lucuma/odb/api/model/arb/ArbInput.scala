// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.arb

import clue.data.{Assign, Ignore, Input, Unassign}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalacheck.Gen.oneOf
import org.scalacheck.rng.Seed

/**
 *
 */
trait ArbInput {

  implicit def arbInput[A: Arbitrary]: Arbitrary[Input[A]] =
    Arbitrary(
      oneOf(
        Gen.const(Ignore),
        Gen.const(Unassign),
        arbitrary[A].map(Assign.apply)
      )
    )

  def arbNotNullableInput[A: Arbitrary]: Arbitrary[Input[A]] =
    Arbitrary(
      oneOf(
        Gen.const(Ignore),
        arbitrary[A].map(Assign.apply)
      )
    )

  implicit def arbInputF[A](implicit fArb: Arbitrary[A => A]): Arbitrary[Input[A] => Input[A]] =
    Arbitrary(
      arbitrary[A => A].map(f => _.map(f))
    )

  implicit def cogInput[A](implicit A: Cogen[A]): Cogen[Input[A]] =
    Cogen[Input[A]] { (seed: Seed, i: Input[A]) =>
      i.fold(seed, seed.next, A.perturb(seed, _))
    }

}

object ArbInput extends ArbInput

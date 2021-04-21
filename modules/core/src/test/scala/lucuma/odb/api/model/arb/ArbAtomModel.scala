// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import cats.data.NonEmptyList
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary


trait ArbAtomModel extends Helper {

  import ArbStepModel._

  implicit def arbAtom[A: Arbitrary]: Arbitrary[AtomModel[A]] =
    Arbitrary {
      for {
        s0 <- arbitrary[StepModel[A]]
        s  <- tinyPositiveSize
        ss <- Gen.listOfN(s, arbitrary[StepModel[A]])
      } yield AtomModel(NonEmptyList(s0, ss))
    }

  implicit def cogAtom[A: Cogen]: Cogen[AtomModel[A]] =
    Cogen[List[StepModel[A]]].contramap(_.steps.toList)


  implicit def arbCreateAtom[A: Arbitrary]: Arbitrary[AtomModel.Create[A]] =
    Arbitrary {
      for {
        s0 <- arbitrary[StepModel.Create[A]]
        s  <- tinyPositiveSize
        ss <- Gen.listOfN(s, arbitrary[StepModel.Create[A]])
      } yield AtomModel.Create[A](s0 :: ss)
    }

  implicit def cogAtomCreate[A: Cogen]: Cogen[AtomModel.Create[A]] =
    Cogen[List[StepModel.Create[A]]].contramap(_.steps)

}

object ArbAtomModel extends ArbAtomModel

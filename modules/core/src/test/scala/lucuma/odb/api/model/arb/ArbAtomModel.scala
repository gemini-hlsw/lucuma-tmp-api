// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.core.model.Atom
import lucuma.core.util.arb.ArbGid

import cats.data.NonEmptyList
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary


trait ArbAtomModel extends Helper {

  import ArbGid._
  import ArbStepModel._

  implicit def arbAtom[A: Arbitrary]: Arbitrary[AtomModel[StepModel[A]]] =
    Arbitrary {
      for {
        id <- arbitrary[Atom.Id]
        s0 <- arbitrary[StepModel[A]]
        s  <- tinyPositiveSize
        ss <- Gen.listOfN(s, arbitrary[StepModel[A]])
      } yield AtomModel(id, NonEmptyList(s0, ss))
    }

  implicit def cogAtom[A: Cogen]: Cogen[AtomModel[StepModel[A]]] =
    Cogen[(Atom.Id, List[StepModel[A]])].contramap { in => (
      in.id,
      in.steps.toList
    )}


  implicit def arbCreateAtom[A: Arbitrary]: Arbitrary[AtomModel.Create[A]] =
    Arbitrary {
      for {
        id <- arbitrary[Option[Atom.Id]]
        s0 <- arbitrary[StepModel.Create[A]]
        s  <- tinyPositiveSize
        ss <- Gen.listOfN(s, arbitrary[StepModel.Create[A]])
      } yield AtomModel.Create[A](id, s0 :: ss)
    }

  def arbValidCreateAtom[A: Arbitrary]: Arbitrary[AtomModel.Create[A]] =
    Arbitrary {
      for {
        s0 <- arbValidStepModelCreate[A].arbitrary
        s  <- tinyPositiveSize
        ss <- Gen.listOfN(s, arbValidStepModelCreate[A].arbitrary)
      } yield AtomModel.Create[A](None, s0 :: ss)
    }

  implicit def cogAtomCreate[A: Cogen]: Cogen[AtomModel.Create[A]] =
    Cogen[(Option[Atom.Id], List[StepModel.Create[A]])].contramap { in => (
      in.id,
      in.steps
    )}

}

object ArbAtomModel extends ArbAtomModel

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import cats.data.NonEmptyList
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbSequenceModel extends Helper {

  import ArbEnumerated._
  import ArbStepModel._

  implicit def arbBreakpointStep[A: Arbitrary]: Arbitrary[SequenceModel.BreakpointStep[A]] =
    Arbitrary {
      for {
        b <- arbitrary[SequenceModel.Breakpoint]
        s <- arbitrary[StepModel[A]]
      } yield SequenceModel.BreakpointStep(b, s)
    }

  implicit def cogBreakpointStep[A: Cogen]: Cogen[SequenceModel.BreakpointStep[A]] =
    Cogen[(SequenceModel.Breakpoint, StepModel[A])].contramap { in => (
      in.breakpoint,
      in.step
    )}

  implicit def arbCreateBreakpointStep[A: Arbitrary]: Arbitrary[SequenceModel.CreateBreakpointStep[A]] =
    Arbitrary {
      for {
        b <- arbitrary[SequenceModel.Breakpoint]
        s <- arbitrary[StepModel.CreateStep[A]]
      } yield SequenceModel.CreateBreakpointStep(b, s)
    }

  implicit def cogCreateBreakpointStep[A: Cogen]: Cogen[SequenceModel.CreateBreakpointStep[A]] =
    Cogen[(SequenceModel.Breakpoint, StepModel.CreateStep[A])].contramap { in => (
      in.breakpoint,
      in.step
    )}

  implicit def arbAtom[A: Arbitrary]: Arbitrary[SequenceModel.Atom[A]] =
    Arbitrary {
      for {
        s0 <- arbitrary[SequenceModel.BreakpointStep[A]]
        s  <- tinyPositiveSize
        ss <- Gen.listOfN(s, arbitrary[SequenceModel.BreakpointStep[A]])
      } yield SequenceModel.Atom(NonEmptyList(s0, ss))
    }

  implicit def cogAtom[A: Cogen]: Cogen[SequenceModel.Atom[A]] =
    Cogen[List[SequenceModel.BreakpointStep[A]]].contramap(_.steps.toList)


  implicit def arbCreateAtom[A: Arbitrary]: Arbitrary[SequenceModel.CreateAtom[A]] =
    Arbitrary {
      for {
        s0 <- arbitrary[SequenceModel.CreateBreakpointStep[A]]
        s  <- tinyPositiveSize
        ss <- Gen.listOfN(s, arbitrary[SequenceModel.CreateBreakpointStep[A]])
      } yield SequenceModel.CreateAtom[A](s0 :: ss)
    }

  implicit def cogCreateAtom[A: Cogen]: Cogen[SequenceModel.CreateAtom[A]] =
    Cogen[List[SequenceModel.CreateBreakpointStep[A]]].contramap(_.steps)

}

object ArbSequenceModel extends ArbSequenceModel

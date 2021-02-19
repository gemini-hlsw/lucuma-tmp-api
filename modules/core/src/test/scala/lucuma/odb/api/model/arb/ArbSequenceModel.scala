// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.odb.api.model.SequenceModel._

import cats.data.NonEmptyList
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbSequenceModel extends Helper {

  import ArbEnumerated._
  import ArbStepModel._

  implicit def arbBreakpointStep[A: Arbitrary]: Arbitrary[BreakpointStep[A]] =
    Arbitrary {
      for {
        b <- arbitrary[Breakpoint]
        s <- arbitrary[StepModel[A]]
      } yield BreakpointStep(b, s)
    }

  implicit def cogBreakpointStep[A: Cogen]: Cogen[BreakpointStep[A]] =
    Cogen[(Breakpoint, StepModel[A])].contramap { in => (
      in.breakpoint,
      in.step
    )}

  implicit def arbBreakpointStepCreate[A: Arbitrary]: Arbitrary[BreakpointStep.Create[A]] =
    Arbitrary {
      for {
        b <- arbitrary[Breakpoint]
        s <- arbitrary[StepModel.CreateStep[A]]
      } yield BreakpointStep.Create(b, s)
    }

  implicit def cogBreakpointStepCreate[A: Cogen]: Cogen[BreakpointStep.Create[A]] =
    Cogen[(Breakpoint, StepModel.CreateStep[A])].contramap { in => (
      in.breakpoint,
      in.step
    )}

  implicit def arbAtom[A: Arbitrary]: Arbitrary[Atom[A]] =
    Arbitrary {
      for {
        s0 <- arbitrary[BreakpointStep[A]]
        s  <- tinyPositiveSize
        ss <- Gen.listOfN(s, arbitrary[BreakpointStep[A]])
      } yield Atom(NonEmptyList(s0, ss))
    }

  implicit def cogAtom[A: Cogen]: Cogen[Atom[A]] =
    Cogen[List[BreakpointStep[A]]].contramap(_.steps.toList)


  implicit def arbCreateAtom[A: Arbitrary]: Arbitrary[Atom.Create[A]] =
    Arbitrary {
      for {
        s0 <- arbitrary[BreakpointStep.Create[A]]
        s  <- tinyPositiveSize
        ss <- Gen.listOfN(s, arbitrary[BreakpointStep.Create[A]])
      } yield Atom.Create[A](s0 :: ss)
    }

  implicit def cogAtomCreate[A: Cogen]: Cogen[Atom.Create[A]] =
    Cogen[List[BreakpointStep.Create[A]]].contramap(_.steps)

  implicit def arbSequence[S: Arbitrary, D: Arbitrary]: Arbitrary[Sequence[S, D]] =
    Arbitrary {
      for {
        st <- arbitrary[S]
        a  <- smallSize
        aq <- Gen.listOfN(a, arbitrary[Atom[D]])
        s  <- smallSize
        sc <- Gen.listOfN(s, arbitrary[Atom[D]])
      } yield Sequence(st, aq, sc)
    }

  implicit def cogSequence[S: Cogen, D: Cogen]: Cogen[Sequence[S, D]] =
    Cogen[(
      S,
      List[Atom[D]],
      List[Atom[D]]
    )].contramap { in => (
      in.static,
      in.acquisition,
      in.science
    )}

  implicit def arbSequenceCreate[S: Arbitrary, D: Arbitrary]: Arbitrary[Sequence.Create[S, D]] =
    Arbitrary {
      for {
        st <- arbitrary[S]
        a  <- smallSize
        aq <- Gen.listOfN(a, arbitrary[Atom.Create[D]])
        s  <- smallSize
        sc <- Gen.listOfN(s, arbitrary[Atom.Create[D]])
      } yield Sequence.Create(st, aq, sc)
    }

  implicit def cogSequenceCreate[S: Cogen, D: Cogen]: Cogen[Sequence.Create[S, D]] =
    Cogen[(
      S,
      List[Atom.Create[D]],
      List[Atom.Create[D]]
    )].contramap { in => (
      in.static,
      in.acquisition,
      in.science
    )}

}

object ArbSequenceModel extends ArbSequenceModel

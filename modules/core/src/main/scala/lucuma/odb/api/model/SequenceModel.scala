// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.util.Enumerated
import lucuma.odb.api.model.StepModel.CreateStep
import cats.Eq
import cats.data.NonEmptyList
import cats.syntax.all._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import monocle.Iso
import monocle.macros.Lenses

object SequenceModel {

  sealed trait Breakpoint extends Product with Serializable {

    def enabled: Boolean =
      this match {
        case Breakpoint.Enabled  => true
        case Breakpoint.Disabled => false
      }

  }

  object Breakpoint {

    case object Enabled  extends Breakpoint
    case object Disabled extends Breakpoint

    val enabled: Breakpoint =
      Enabled

    val disabled: Breakpoint =
      Disabled

    val fromBoolean: Iso[Boolean, Breakpoint] =
      Iso[Boolean, Breakpoint](b => if (b) Enabled else Disabled)(_.enabled)

    implicit val EnumeratedBreakpoint: Enumerated[Breakpoint] =
      Enumerated.of(enabled, disabled)

    implicit val DecoderBreakpoint: Decoder[Breakpoint] =
      deriveDecoder[Breakpoint]
  }


  @Lenses final case class BreakpointStep[A](
    breakpoint: Breakpoint,
    step:       StepModel[A]
  )

  object BreakpointStep {

    implicit def EqBreakpointStep[A: Eq]: Eq[BreakpointStep[A]] =
      Eq.by { a => (
        a.breakpoint,
        a.step
      )}

  }

  @Lenses final case class CreateBreakpointStep[A](
    breakpoint: Breakpoint,
    step:       CreateStep[A]
  ) {

    def create[B](implicit V: InputValidator[A, B]): ValidatedInput[BreakpointStep[B]] =
      step.create[B].map(s => BreakpointStep(breakpoint, s))

  }

  object CreateBreakpointStep {

    def stopBefore[A](s: CreateStep[A]): CreateBreakpointStep[A] =
      CreateBreakpointStep(Breakpoint.enabled, s)

    def continueTo[A](s: CreateStep[A]): CreateBreakpointStep[A] =
      CreateBreakpointStep(Breakpoint.disabled, s)

    implicit def EqCreateBreakpointStep[A: Eq]: Eq[CreateBreakpointStep[A]] =
      Eq.by { a => (
        a.breakpoint,
        a.step
      )}

    implicit def DecoderCreateBreakpointStep[A: Decoder]: Decoder[CreateBreakpointStep[A]] =
      deriveDecoder[CreateBreakpointStep[A]]

    implicit def ValidatorCreateBreakpointStep[A, B](implicit V: InputValidator[A, B]): InputValidator[CreateBreakpointStep[A], BreakpointStep[B]] =
      (cbs: CreateBreakpointStep[A]) => cbs.create[B]
  }

  @Lenses final case class Atom[A](
    steps: NonEmptyList[BreakpointStep[A]]
  )


  object Atom {

    def one[A](head: BreakpointStep[A]): Atom[A] =
      Atom(NonEmptyList.one(head))

    def ofSteps[A](head: BreakpointStep[A], tail: BreakpointStep[A]*): Atom[A] =
      Atom(NonEmptyList.of(head, tail: _*))

    def fromNel[A]: Iso[NonEmptyList[BreakpointStep[A]], Atom[A]] =
      Iso[NonEmptyList[BreakpointStep[A]], Atom[A]](nel => Atom(nel))(_.steps)

    implicit def EqSequenceAtom[A: Eq]: Eq[Atom[A]] =
      Eq.by(_.steps)

  }

  @Lenses final case class CreateAtom[A](
    steps: List[CreateBreakpointStep[A]]
  ) {

    def create[B](implicit V: InputValidator[A, B]): ValidatedInput[Atom[B]] =
      steps match {
        case Nil    =>
          InputError.fromMessage("Cannot create an empty SequenceAtom").invalidNec[Atom[B]]

        case h :: t =>
          (h.create[B], t.traverse(_.create[B])).mapN { (h0, t0) =>
            Atom.fromNel.get(NonEmptyList(h0, t0))
          }

      }
  }

  object CreateAtom {

    def singleton[A](step: CreateBreakpointStep[A]): CreateAtom[A] =
      CreateAtom(List(step))

    def stopBefore[A](step: CreateStep[A]): CreateAtom[A] =
      singleton(CreateBreakpointStep.stopBefore(step))

    def continueTo[A](step: CreateStep[A]): CreateAtom[A] =
      singleton(CreateBreakpointStep.continueTo(step))

    implicit def DecoderCreateSequenceAtom[A: Decoder]: Decoder[CreateAtom[A]] =
      deriveDecoder[CreateAtom[A]]

    implicit def ValidatorCreateSequenceAtom[A, B](implicit V: InputValidator[A, B]): InputValidator[CreateAtom[A], Atom[B]] =
      (csa: CreateAtom[A]) => csa.create[B]

    implicit def EqCreateSequenceAtom[A: Eq]: Eq[CreateAtom[A]] =
      Eq.by(_.steps)

  }


}

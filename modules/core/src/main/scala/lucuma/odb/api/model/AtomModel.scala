// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.StepConfig.CreateStepConfig

import cats.Eq
import cats.data.NonEmptyList
import cats.syntax.all._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import monocle.Iso
import monocle.macros.Lenses

@Lenses final case class AtomModel[A](
  steps: NonEmptyList[StepModel[A]]
)

object AtomModel {
  def one[A](head: StepModel[A]): AtomModel[A] =
    AtomModel(NonEmptyList.one(head))

  def ofSteps[A](head: StepModel[A], tail: StepModel[A]*): AtomModel[A] =
    AtomModel(NonEmptyList.of(head, tail: _*))

  def fromNel[A]: Iso[NonEmptyList[StepModel[A]], AtomModel[A]] =
    Iso[NonEmptyList[StepModel[A]], AtomModel[A]](nel => AtomModel(nel))(_.steps)

  implicit def EqSequenceAtom[A: Eq]: Eq[AtomModel[A]] =
    Eq.by(_.steps)


  @Lenses final case class Create[A](
    steps: List[StepModel.Create[A]]
  ) {

    def create[B](implicit V: InputValidator[A, B]): ValidatedInput[AtomModel[B]] =
      steps match {
        case Nil    =>
          InputError.fromMessage("Cannot create an empty sequence atom").invalidNec[AtomModel[B]]

        case h :: t =>
          (h.create[B], t.traverse(_.create[B])).mapN { (h0, t0) =>
            AtomModel.fromNel.get(NonEmptyList(h0, t0))
          }
      }
  }

  object Create {

    def singleton[A](step: StepModel.Create[A]): Create[A] =
      Create(List(step))

    def stopBefore[A](step: CreateStepConfig[A]): Create[A] =
      singleton(StepModel.Create.stopBefore(step))

    def continueTo[A](step: CreateStepConfig[A]): Create[A] =
      singleton(StepModel.Create.continueTo(step))

    implicit def DecoderCreate[A: Decoder]: Decoder[Create[A]] =
      deriveDecoder[Create[A]]

    implicit def ValidatorCreate[A, B](implicit V: InputValidator[A, B]): InputValidator[Create[A], AtomModel[B]] =
      (csa: Create[A]) => csa.create[B]

    implicit def EqCreate[A: Eq]: Eq[Create[A]] =
      Eq.by(_.steps)

  }

}
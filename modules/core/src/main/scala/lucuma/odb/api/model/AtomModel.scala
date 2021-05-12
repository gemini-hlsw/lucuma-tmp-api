// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.model.Atom
import lucuma.odb.api.model.StepConfig.CreateStepConfig

import cats.Eq
import cats.data.{NonEmptyList, State}
import cats.syntax.all._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import monocle.macros.Lenses

@Lenses final case class AtomModel[A](
  id:    Atom.Id,
  steps: NonEmptyList[StepModel[A]]
)

object AtomModel {
  def one[A](id: Atom.Id, head: StepModel[A]): AtomModel[A] =
    AtomModel(id, NonEmptyList.one(head))

  def ofSteps[A](id: Atom.Id, head: StepModel[A], tail: StepModel[A]*): AtomModel[A] =
    AtomModel(id, NonEmptyList.of(head, tail: _*))

  implicit def EqSequenceAtom[A: Eq]: Eq[AtomModel[A]] =
    Eq.by { a => (
      a.id,
      a.steps
    )}

  @Lenses final case class Create[A](
    id:    Option[Atom.Id],
    steps: List[StepModel.Create[A]]
  ) {

    def create[T, B](db: Database[T])(implicit V: InputValidator[A, B]): State[T, ValidatedInput[AtomModel[B]]] =
      steps match {
        case Nil    =>
          db.error[AtomModel[B]]("Cannot create an empty sequence atom")

        case h :: t =>
          for {
            i  <- db.atom.getUnusedId(id)
            hʹ <- h.create[T, B](db)
            tʹ <- t.traverse(_.create[T, B](db))
            a   = (i, hʹ, tʹ.sequence).mapN { (iʹʹ, hʹʹ, tʹʹ) =>
              AtomModel.ofSteps(iʹʹ, hʹʹ, tʹʹ: _*)
            }
            _  <- db.atom.saveValid(a)(_.id)
          } yield a

      }
  }

  object Create {

    def singleton[A](step: StepModel.Create[A]): Create[A] =
      Create(None, List(step))

    def stopBefore[A](step: CreateStepConfig[A]): Create[A] =
      singleton(StepModel.Create.stopBefore(step))

    def continueTo[A](step: CreateStepConfig[A]): Create[A] =
      singleton(StepModel.Create.continueTo(step))

    implicit def DecoderCreate[A: Decoder]: Decoder[Create[A]] =
      deriveDecoder[Create[A]]

    implicit def EqCreate[A: Eq]: Eq[Create[A]] =
      Eq.by { a => (
        a.id,
        a.steps
      )}

  }

}
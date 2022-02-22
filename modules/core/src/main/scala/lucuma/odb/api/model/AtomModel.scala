// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.data.NonEmptyList
import cats.effect.Sync
import cats.syntax.all._
import cats.{Applicative, Eq, Eval, Traverse}
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.odb.api.model.StepConfig.CreateStepConfig

final case class AtomModel[A](
  id:    Atom.Id,
  steps: NonEmptyList[A]
)

object AtomModel {

  def one[A](id: Atom.Id, head: A): AtomModel[A] =
    AtomModel(id, NonEmptyList.one(head))

  def ofSteps[A](id: Atom.Id, head: A, tail: A*): AtomModel[A] =
    AtomModel(id, NonEmptyList.of(head, tail: _*))

  implicit def EqAtomModel[A: Eq]: Eq[AtomModel[A]] =
    Eq.by { a => (
      a.id,
      a.steps
    )}

  implicit val TraverseAtomModel: Traverse[AtomModel] =
    new Traverse[AtomModel] {
      override def traverse[G[_], A, B](fa: AtomModel[A])(f: A => G[B])(implicit ev: Applicative[G]): G[AtomModel[B]] =
        fa.steps.traverse(f).map { steps => AtomModel(fa.id, steps) }

      override def foldLeft[A, B](fa: AtomModel[A], b: B)(f: (B, A) => B): B =
        fa.steps.foldLeft(b)(f)

      override def foldRight[A, B](fa: AtomModel[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.steps.foldRight(lb)(f)
    }

  final case class Create[A](
    steps: List[StepModel.Create[A]]
  ) {

    def create[F[_]: Sync, B](implicit V: InputValidator[A, B]): F[ValidatedInput[AtomModel[StepModel[B]]]] =
      steps match {

        case Nil    =>
          Sync[F].pure(
            InputError.fromMessage("Cannot create an emptySequence atom").invalidNec
          )

        case h :: t =>
          for {
            a  <- Atom.Id.random[F]
            hʹ <- h.create
            tʹ <- t.traverse(_.create).map(_.sequence)
          } yield (hʹ, tʹ).mapN((hʹʹ, tʹʹ) => AtomModel.ofSteps(a, hʹʹ, tʹʹ: _*))
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

    implicit def EqCreate[A: Eq]: Eq[Create[A]] =
      Eq.by(_.steps)

  }

}

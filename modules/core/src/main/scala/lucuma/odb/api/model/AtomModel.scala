// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.model.{Atom, Step}
import lucuma.odb.api.model.StepConfig.CreateStepConfig
import cats.{Applicative, Eq, Eval, Traverse}
import cats.data.{NonEmptyList, StateT}
import cats.syntax.all._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

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

  def dereference[D](id: Atom.Id)(f: StepConfig[_] => Option[D]): StateT[EitherInput, Database, Option[AtomModel[StepModel[D]]]] =
      Database
        .atom
        .lookupOption(id)
        .flatMap(_.flatTraverse(_.dereference[D](f)))

  implicit class ReferenceExtensions(a: AtomModel[Step.Id]) {

    def dereference[D](f: StepConfig[_] => Option[D]): StateT[EitherInput, Database, Option[AtomModel[StepModel[D]]]] =
      a.steps
       .traverse(i => StepModel.dereference[D](i)(f))
       .map(_.sequence.map(AtomModel(a.id, _)))

  }


  final case class Create[A](
    id:    Option[Atom.Id],
    steps: List[StepModel.Create[A]]
  ) {

    def create[B](implicit V: InputValidator[A, B]): StateT[EitherInput, Database, AtomModel[StepModel[B]]] =
      steps match {

        case Nil    =>
          StateT.liftF[EitherInput, Database, AtomModel[StepModel[B]]](
            InputError.fromMessage("Cannot create an emptySequence atom").leftNec[AtomModel[StepModel[B]]]
          )

        case h :: t =>
          for {
            i  <- Database.atom.getUnusedKey(id)
            hʹ <- h.create
            tʹ <- t.traverse(_.create[B])
            a   = AtomModel.ofSteps(i, hʹ, tʹ: _*)
            _  <- Database.atom.saveNew(i, a.map(_.id))
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

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.model.Atom

import cats.{Applicative, Eq, Eval, Monad, Traverse}
import cats.mtl.Stateful
import cats.syntax.all._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import monocle.macros.Lenses

/**
 * Sequence representation.
 *
 * @param atoms atoms that make up the sequence
 *
 * @tparam A atom type
 */
@Lenses final case class SequenceModel[A](
  atoms: List[A]
)

object SequenceModel {

  implicit val TraverseSequenceModel: Traverse[SequenceModel] =
    new Traverse[SequenceModel] {
      override def traverse[G[_], A, B](fa: SequenceModel[A])(f: A => G[B])(implicit evidence$1: Applicative[G]): G[SequenceModel[B]] =
        fa.atoms.traverse(f).map { atoms => SequenceModel(atoms) }

      override def foldLeft[A, B](fa: SequenceModel[A], b: B)(f: (B, A) => B): B =
        fa.atoms.foldLeft(b)(f)

      override def foldRight[A, B](fa: SequenceModel[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.atoms.foldr(lb)(f)

    }

  implicit def EqSequenceModel[A: Eq]: Eq[SequenceModel[A]] =
    Eq.by { _.atoms }

  implicit class ReferenceExtensions(s: SequenceModel[Atom.Id]) {

    def dereference[F[_]: Monad, T, D](db: DatabaseReader[T])(f: StepConfig[_] => Option[D])(implicit S: Stateful[F, T]): F[Option[DereferencedSequence[D]]] =
      s.atoms
       .traverse(AtomModel.dereference[F, T, D](db, _)(f))
       .map(_.sequence.map(SequenceModel(_)))
  }

  final case class Create[CD](
    atoms: List[AtomModel.Create[CD]]
  ) {

    def create[F[_]: Monad, T, D](db: DatabaseState[T])(implicit ev: InputValidator[CD, D], S: Stateful[F, T]): F[ValidatedInput[DereferencedSequence[D]]] =
      atoms.traverse(_.create[F, T, D](db)).map(_.sequence.map(SequenceModel(_)))
  }

  object Create {

    implicit def EdCreate[D: Eq]: Eq[Create[D]] =
      Eq.by { _.atoms }

    implicit def DecoderCreate[D: Decoder]:Decoder[Create[D]] =
      deriveDecoder[Create[D]]

  }

}

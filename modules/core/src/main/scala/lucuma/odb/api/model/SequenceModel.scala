// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.data.StateT
import lucuma.core.model.Atom
import cats.{Applicative, Eq, Eval, Traverse}
import cats.syntax.all._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.core.util.Enumerated

/**
 * Sequence representation.
 *
 * @param atoms atoms that make up the sequence
 *
 * @tparam A atom type
 */
final case class SequenceModel[A](
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

    def dereference[D](f: StepConfig[_] => Option[D]): StateT[EitherInput, Database, Option[DereferencedSequence[D]]] =
      s.atoms
       .traverse(AtomModel.dereference[D](_)(f))
       .map(_.sequence.map(SequenceModel(_)))
  }

  final case class Create[CD](
    atoms: List[AtomModel.Create[CD]]
  ) {

    def create2[D](implicit ev: InputValidator[CD, D]): StateT[EitherInput, Database, DereferencedSequence[D]] =
      atoms.traverse(_.create2[D]).map(SequenceModel(_))
  }

  object Create {

    implicit def EdCreate[D: Eq]: Eq[Create[D]] =
      Eq.by { _.atoms }

    implicit def DecoderCreate[D: Decoder]:Decoder[Create[D]] =
      deriveDecoder[Create[D]]

  }

  sealed abstract class SequenceType(
    val tag:       String,
    val shortName: String
  ) extends Product with Serializable

  object SequenceType {
    case object Acquisition extends SequenceType("ACQUISITION", "Acquisition")
    case object Science     extends SequenceType("SCIENCE", "Science")

    val all: List[SequenceType] =
      List(
        Acquisition,
        Science
      )

    def fromTag(s: String): Option[SequenceType] =
      all.find(_.tag === s)

    def unsafeFromTag(s: String): SequenceType =
      fromTag(s).getOrElse(throw new NoSuchElementException(s"SequenceType: Invalid tag: '$s'"))

    implicit val EnumeratedSequenceType: Enumerated[SequenceType] =
      new Enumerated[SequenceType] {
        override def all: List[SequenceType] = SequenceType.all
        override def tag(a: SequenceType): String = a.tag
        override def unsafeFromTag(s: String): SequenceType = SequenceType.unsafeFromTag(s)
      }

  }



}

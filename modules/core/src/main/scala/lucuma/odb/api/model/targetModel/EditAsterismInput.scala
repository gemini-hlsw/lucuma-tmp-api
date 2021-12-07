// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.data.{Nested, State}
import cats.mtl.Stateful
import cats.{Eq, Monad}
import cats.syntax.functor._
import cats.syntax.option._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.core.model.Target
import lucuma.core.optics.state.all._
import lucuma.odb.api.model.{DatabaseState, ValidatedInput}

import scala.collection.immutable.SortedSet

final case class EditAsterismInput(
  add:    Option[Target.Id],
  delete: Option[Target.Id]
) {

  private def updateAsterism[F[_]: Monad, T](
    db:  DatabaseState[T],
    tid: Target.Id
  )(
    op:  (SortedSet[Target.Id], Target.Id) => SortedSet[Target.Id]
  )(implicit S: Stateful[F, T]): F[ValidatedInput[State[TargetEnvironmentModel, Unit]]] =
    Nested(db.target.lookupValidated(tid)).as {
      TargetEnvironmentModel.asterism.mod_(op(_, tid))
    }.value

  def editor[F[_]: Monad, T](
    db: DatabaseState[T]
  )(implicit S: Stateful[F, T]): F[ValidatedInput[State[TargetEnvironmentModel, Unit]]] =
    ValidatedInput.requireOneF("edit",
      add.map(tid => updateAsterism(db, tid)(_ + _)),
      delete.map(tid => updateAsterism(db, tid)(_ + _))
    )

}

object EditAsterismInput {

  implicit val DecoderEditTargetInput: Decoder[EditAsterismInput] =
    deriveDecoder[EditAsterismInput]

  implicit val EqEditTargetInput: Eq[EditAsterismInput] =
    Eq.by { a => (
      a.add,
      a.delete
    )}

  val Empty: EditAsterismInput =
    EditAsterismInput(None, None)

  def add(tid: Target.Id): EditAsterismInput =
    Empty.copy(add = tid.some)

  def delete(tid: Target.Id): EditAsterismInput =
    Empty.copy(delete = tid.some)

}

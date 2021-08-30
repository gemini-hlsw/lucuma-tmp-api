// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.{Eq, Monad}
import cats.mtl.Stateful
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.odb.api.model.{DatabaseState, ValidatedInput}
import lucuma.odb.api.model.syntax.validatedinput._

final case class BulkEditTargetInput(
  select:          Option[SelectTargetEnvironmentInput],

  addSidereal:     Option[CreateSiderealInput],
  addNonsidereal:  Option[CreateNonsiderealInput],
  editSidereal:    Option[EditSiderealInput],
  editNonsidereal: Option[EditNonsiderealInput],
  delete:          Option[SelectTargetInput]
) extends EditTargetAction {

  def edit[F[_]: Monad, T](
    db: DatabaseState[T]
  )(implicit S: Stateful[F, T]): F[ValidatedInput[List[TargetListEditResult]]] =
    TargetListEditResult.fromTargetEditDescs(
      db,
      for {
        s <- SelectTargetEnvironmentInput.ids(db, select)
        e <- s.traverse(editEnv(db, _))
      } yield e.flatten
    )

}

object BulkEditTargetInput {

  implicit val DecoderBulkEditTargetInput: Decoder[BulkEditTargetInput] =
    deriveDecoder[BulkEditTargetInput]

  implicit val EqBulkEditTarget: Eq[BulkEditTargetInput] =
    Eq.by { a => (
      a.addSidereal,
      a.addNonsidereal,
      a.editSidereal,
      a.editNonsidereal,
      a.delete
    )}

  val Empty: BulkEditTargetInput =
    BulkEditTargetInput(None, None, None, None, None, None)

  def addSidereal(s: Option[SelectTargetEnvironmentInput], c: CreateSiderealInput): BulkEditTargetInput =
    Empty.copy(select = s, addSidereal = c.some)

  def addNonsidereal(s: Option[SelectTargetEnvironmentInput], c: CreateNonsiderealInput): BulkEditTargetInput =
    Empty.copy(select = s, addNonsidereal = c.some)

  def editSidereal(s: Option[SelectTargetEnvironmentInput], e: EditSiderealInput): BulkEditTargetInput =
    Empty.copy(select = s, editSidereal = e.some)

  def editNonsidereal(s: Option[SelectTargetEnvironmentInput], e: EditNonsiderealInput): BulkEditTargetInput =
    Empty.copy(select = s, editNonsidereal = e.some)

  def delete(s: Option[SelectTargetEnvironmentInput], d: SelectTargetInput): BulkEditTargetInput =
    Empty.copy(select = s, delete = d.some)

}

// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.{Eq, Monad}
import cats.mtl.Stateful
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.odb.api.model.{DatabaseState, ValidatedInput}
import lucuma.odb.api.model.syntax.validatedinput._

final case class BulkEditTargetListInput(
  select: Option[SelectTargetEnvironmentInput],
  edits:  List[EditTargetInput]
) {

  def edit[F[_]: Monad, T](
    db:  DatabaseState[T]
  )(implicit S: Stateful[F, T]): F[ValidatedInput[List[TargetListEditResult]]] =
    TargetListEditResult.fromTargetEditDescs(
      db,
      for {
        s <- SelectTargetEnvironmentInput.ids(db, select)
        e <- s.traverse(ids => edits.traverse(_.editEnv(db, ids))).map(_.map(_.flatSequence))
      } yield e.flatten
    )

}

object BulkEditTargetListInput {

  implicit val DecoderBulkEditTargetListInput: Decoder[BulkEditTargetListInput] =
    deriveDecoder[BulkEditTargetListInput]

  implicit val EqBulkEditTargetListInput: Eq[BulkEditTargetListInput] =
    Eq.by { a => (
      a.select,
      a.edits
    )}

}


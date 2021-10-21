// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.{Eq, Monad}
import cats.mtl.Stateful
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.odb.api.model.{DatabaseState, ValidatedInput}
import lucuma.odb.api.model.syntax.validatedinput._

import scala.collection.immutable.SortedSet


final case class BulkReplaceTargetListInput(
  select:  SelectTargetEnvironmentInput,
  replace: List[CreateTargetInput]
) {

  def replace[F[_]: Monad, T](
    db:  DatabaseState[T]
  )(implicit S: Stateful[F, T]): F[ValidatedInput[List[TargetListEditResult]]] = {

    def deleteTargets(vs: SortedSet[TargetEnvironment.Id]): F[List[TargetEditResult]] =
      for {
        tms <- db.target.findAll { case (_, tm) => vs(tm.targetEnvironmentId) }
        res <- tms.traverse(tm => db.target.delete(tm.id)).as(tms.map(TargetEditResult.delete))
      } yield res

    def createTargets(vs: SortedSet[TargetEnvironment.Id]): F[ValidatedInput[List[TargetEditResult]]] =
      replace
        .traverse(cti => vs.toList.traverse(cti.create(db, _)))
        .map(_.flatten.sequence.map(_.map(TargetEditResult.create)))

    TargetListEditResult.fromTargetEditDescs(
      db,
      for {
        s <- select.selectIds(db)
        d <- s.traverse(deleteTargets)
        e <- s.traverse(createTargets)
      } yield (d, e.flatten).mapN(_ ++ _)
    )
  }

}

object BulkReplaceTargetListInput {

  implicit val DecoderBulkReplaceTargetListInput: Decoder[BulkReplaceTargetListInput] =
    deriveDecoder[BulkReplaceTargetListInput]

  implicit val EqBulkReplaceTargetListInput: Eq[BulkReplaceTargetListInput] =
    Eq.by { a => (
      a.select,
      a.replace
    )}

}
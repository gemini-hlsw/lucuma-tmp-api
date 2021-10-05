// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import lucuma.odb.api.model.{DatabaseState, ObservationModel, ProgramModel, ValidatedInput}
import lucuma.odb.api.model.syntax.validatedinput._
import cats.Monad
import cats.data.Nested
import cats.implicits.catsKernelOrderingForOrder
import cats.mtl.Stateful
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._


/**
 * Describes the edits that were performed to a target list in a target
 * environment.
 */
final case class TargetListEditResult(
  targetEnvironment: TargetEnvironmentModel,
  observation:       Option[ObservationModel],
  program:           ProgramModel,
  edits:             List[TargetEditResult]
) extends TargetEnvironmentContext

object TargetListEditResult {

  private def groupAndMap[F[_]: Monad, T](
    db:    DatabaseState[T],
    edits: List[TargetEditResult]
  )(implicit S: Stateful[F, T]): F[ValidatedInput[List[TargetListEditResult]]] =
    edits
      .groupBy(_.target.targetEnvironmentId)
      .toList
      .traverse { case (vid, edits) =>
        Nested(TargetEnvironmentContext.fromId(db, vid)).map { rʹ =>
          TargetListEditResult(rʹ.targetEnvironment, rʹ.observation, rʹ.program, edits.sortBy(_.target.id))
        }.value
      }.map(_.sequence.map(_.sortBy(_.targetEnvironment.id)))

  def fromTargetEditDescs[F[_]: Monad, T](
    db:    DatabaseState[T],
    edits: F[ValidatedInput[List[TargetEditResult]]]
  )(implicit S: Stateful[F, T]):  F[ValidatedInput[List[TargetListEditResult]]] =
    edits.flatMap(_.flatTraverse(groupAndMap(db, _)))

}
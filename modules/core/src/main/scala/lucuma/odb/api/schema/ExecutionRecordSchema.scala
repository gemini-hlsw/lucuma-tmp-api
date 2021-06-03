// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.model.{Observation, Step}
import lucuma.odb.api.repo.OdbRepo

import cats.effect.Effect
import eu.timepit.refined.types.all.PosInt
import lucuma.odb.api.model.{DatasetModel, ExecutionEvent, ExecutionEventModel}
import sangria.schema._

object ExecutionRecordSchema {

  import context._
  import DatasetSchema._
  import ExecutionEventSchema._
  import Paging._

  def ExecutionRecordType[F[_]: Effect]: ObjectType[OdbRepo[F], Observation.Id] =
    ObjectType(
      name     = "ExecutionRecord",
      fieldsFn = () => fields(

        Field(
          name        = "datasets",
          fieldType   = DatasetConnectionType[F],
          description = Some("Datasets associated with the observation"),
          arguments   = List(
            ArgumentPagingFirst,
            ArgumentPagingCursor
          ),
          resolve     = c =>
            unsafeSelectPageFuture[F, (Step.Id, PosInt), DatasetModel](
              c.pagingCursor("(step-id, index)")(StepAndIndexCursor.getOption),
              dm => StepAndIndexCursor.reverseGet((dm.stepId, dm.index)),
              o  => c.ctx.executionEvent.selectDatasetsForObservation(c.value, c.pagingFirst, o)
            )
        ),

        Field(
          name        = "events",
          fieldType   = ExecutionEventConnectionType[F],
          description = Some("Events associated with the observation"),
          arguments   = List(
            ArgumentPagingFirst,
            ArgumentPagingCursor
          ),
          resolve     = c =>
            unsafeSelectPageFuture[F, ExecutionEvent.Id, ExecutionEventModel](
              c.pagingExecutionEventId,
              (e: ExecutionEventModel) => Cursor.gid[ExecutionEvent.Id].reverseGet(e.id),
              // TODO: here we're going to want the events sorted by timestamp, not GID
              eid => c.ctx.executionEvent.selectEventsForObservation(c.value, c.pagingFirst, eid)
            )
        )

      )
    )

}

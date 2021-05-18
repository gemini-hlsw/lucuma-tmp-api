// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.model.Observation
import lucuma.odb.api.repo.OdbRepo
import cats.effect.Effect
import lucuma.odb.api.model.{Dataset, DatasetModel, ExecutionEvent, ExecutionEventModel}
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
            unsafeSelectPageFuture[F, Dataset.Id, DatasetModel](c.pagingDatasetId, (d: DatasetModel) => d.id) { did =>
              c.ctx.dataset.selectDatasetsForObservation(c.value, c.pagingFirst, did)
            }
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
            unsafeSelectPageFuture[F, ExecutionEvent.Id, ExecutionEventModel](c.pagingExecutionEventId, (e: ExecutionEventModel) => e.id) { eid =>
              // TODO: here we're going to want the events sorted by timestamp, not GID
              c.ctx.executionEvent.selectEventsForObservation(c.value, c.pagingFirst, eid)
            }
        )

      )
    )

}

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.all._
import eu.timepit.refined.types.all.PosInt
import lucuma.core.model.{ExecutionEvent, Observation}
import lucuma.odb.api.model.{DatasetModel, ExecutionEventModel, InstrumentConfigModel, Step}
import lucuma.odb.api.repo.OdbCtx
import org.typelevel.log4cats.Logger
import sangria.schema._

object ExecutionSchema {

  import context._
  import DatasetSchema._
  import ExecutionEventSchema._
  import Paging._

  def ExecutionType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], Observation.Id] =
    ObjectType(
      name     = "Execution",
      fieldsFn = () => fields(

        Field(
          name        = "datasets",
          fieldType   = DatasetConnectionType[F],
          description = "Datasets associated with the observation".some,
          arguments   = List(
            ArgumentPagingFirst,
            ArgumentPagingCursor
          ),
          resolve     = c =>
            unsafeSelectPageFuture[F, (Step.Id, PosInt), DatasetModel](
              c.pagingCursor("(step-id, index)")(s => StepAndIndexCursor.getOption(s).flatMap(StepAndIndex.unapply)),
              dm => StepAndIndexCursor.reverseGet(StepAndIndex(dm.stepId, dm.index)),
              o  => c.ctx.odbRepo.executionEvent.selectDatasetsPageForObservation(c.value, c.pagingFirst, o)
            )
        ),

        Field(
          name        = "events",
          fieldType   = ExecutionEventConnectionType[F],
          description = "Events associated with the observation".some,
          arguments   = List(
            ArgumentPagingFirst,
            ArgumentPagingCursor
          ),
          resolve     = c =>
            unsafeSelectPageFuture[F, ExecutionEvent.Id, ExecutionEventModel](
              c.pagingExecutionEventId,
              (e: ExecutionEventModel) => Cursor.gid[ExecutionEvent.Id].reverseGet(e.id),
              eid => c.ctx.odbRepo.executionEvent.selectEventsPageForObservation(c.value, c.pagingFirst, eid)
            )
        ),

        Field(
          name        = "executionConfig",
          fieldType   = OptionType(ExecutionConfigSchema.ExecutionConfigType[F]),
          description = "Execution config".some,
          resolve     = c =>
            c.observation(
              _.selectManualConfig(c.value).map(_.flatMap {
                  case gn: InstrumentConfigModel.GmosNorth =>
                    ExecutionConfigSchema.GmosNorthExecutionContext(
                      c.value,
                      gn.instrument,
                      gn.static,
                      gn.acquisition,
                      gn.science
                    ).some

                  case gs: InstrumentConfigModel.GmosSouth =>
                    ExecutionConfigSchema.GmosSouthExecutionContext(
                      c.value,
                      gs.instrument,
                      gs.static,
                      gs.acquisition,
                      gs.science
                    ).some

                  case _ => none
                }
              )
            )
        )

      )
    )

}

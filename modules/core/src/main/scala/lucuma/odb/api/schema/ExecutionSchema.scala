// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.model.{ExecutionEvent, Observation, Step}
import lucuma.odb.api.model.{DatasetModel, ExecutedStepModel, ExecutionEventModel, InstrumentConfigModel}
import lucuma.odb.api.repo.OdbRepo

import cats.MonadError
import cats.effect.std.Dispatcher
import cats.syntax.all._
import eu.timepit.refined.types.all.PosInt
import sangria.schema._

object ExecutionSchema {

  import context._
  import DatasetSchema._
  import ExecutionEventSchema._
  import ExecutedStepSchema._
  import Paging._

  def ExecutionType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], Observation.Id] =
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
              c.pagingCursor("(step-id, index)")(StepAndIndexCursor.getOption),
              dm => StepAndIndexCursor.reverseGet((dm.stepId, dm.index)),
              o  => c.ctx.executionEvent.selectDatasetsPageForObservation(c.value, c.pagingFirst, o)
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
              eid => c.ctx.executionEvent.selectEventsPageForObservation(c.value, c.pagingFirst, eid)
            )
        ),

        Field(
          name        = "executedSteps",
          fieldType   = ExecutedStepConnectionType[F],
          description = "Executed steps associated with the observation".some,
          arguments   = List(
            ArgumentPagingFirst,
            ArgumentPagingCursor
          ),
          resolve     = c =>
            unsafeSelectPageFuture[F, Step.Id, ExecutedStepModel](
              c.pagingStepId,
              (s: ExecutedStepModel) => Cursor.gid[Step.Id].reverseGet(s.stepId),
              sid => c.ctx.executionEvent.selectExecutedStepsPageForObservation(c.value, c.pagingFirst, sid)
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

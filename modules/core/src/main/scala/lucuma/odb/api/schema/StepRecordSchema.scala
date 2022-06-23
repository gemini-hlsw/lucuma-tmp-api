// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{StepQaState, StepRecord, WhereDatasetInput}
import cats.effect.Async
import cats.effect.kernel.Sync
import cats.effect.std.Dispatcher
import cats.syntax.all._
import lucuma.core.model.ExecutionEvent
import lucuma.odb.api.model.ExecutionEventModel.{DatasetEvent, StepEvent}
import lucuma.odb.api.repo.{OdbCtx, ResultPage}
import lucuma.odb.api.schema.syntax.`enum`._
import org.typelevel.log4cats.Logger
import sangria.schema._

object StepRecordSchema {

  import context._
  import Paging._

  import DatasetSchema.DatasetSelectResult
  import DatasetQuery.ArgumentOptionOffsetDataset
  import ExecutionEventSchema.{DatasetEventConnectionType, StepEventConnectionType}
  import StepSchema.{StepConfigType, StepIdType}
  import TimeSchema.{NonNegativeDurationType, InstantScalar}
  import QuerySchema.ArgumentOptionLimit
  import VisitRecordSchema.VisitIdType

  implicit val EnumTypeStepQaState: EnumType[StepQaState] =
    EnumType.fromEnumerated(
      "StepQaState",
      "Step QA State"
    )

  def StepRecordType[F[_]: Dispatcher: Async: Logger, D](
    typePrefix:  String,
    dynamicType: OutputType[D]
  ): ObjectType[OdbCtx[F], StepRecord.Output[D]] =
    ObjectType(
      name        = s"${typePrefix}StepRecord",
      description = s"A $typePrefix step configuration as recorded by Observe",
      fieldsFn    = () => fields(

        Field(
          name        = "id",
          fieldType   = StepIdType,
          description = "Step id".some,
          resolve     = _.value.stepId
        ),

        Field(
          name        = "visitId",
          fieldType   = VisitIdType,
          description = "Visit id".some,
          resolve     = _.value.visitId
        ),

        Field(
          name        = "created",
          fieldType   = InstantScalar,
          description = "Created by Observe at time".some,
          resolve     = _.value.created
        ),

        Field(
          name        = "startTime",
          fieldType   = OptionType(InstantScalar),
          description = "Started at time".some,
          resolve     = _.value.startTime
        ),

        Field(
          name        = "endTime",
          fieldType   = OptionType(InstantScalar),
          description = "Ended at time".some,
          resolve     = _.value.endTime
        ),

        Field(
          name        = "duration",
          fieldType   = NonNegativeDurationType,
          description = "Step duration".some,
          resolve     = _.value.duration
        ),

        Field(
          name        = "instrumentConfig",
          fieldType   = dynamicType,
          description = s"$typePrefix configuration for this step".some,
          resolve     = _.value.stepConfig.instrumentConfig
        ),

        Field(
          name        = "stepConfig",
          fieldType   = StepConfigType[F],
          description = "The executed step itself".some,
          resolve     = c => c.value.stepConfig
        ),

        Field(
          name        = "stepEvents",
          fieldType   = StepEventConnectionType[F],
          description = "Step events associated with this step".some,
          arguments   = List(
            ArgumentPagingFirst,
            ArgumentPagingCursor
          ),
          resolve     = c =>
            unsafeSelectPageFuture[F, ExecutionEvent.Id, StepEvent](
              c.pagingExecutionEventId,
              s => Cursor.gid[ExecutionEvent.Id].reverseGet(s.id),
              g => Sync[F].delay(
                ResultPage.fromSeq[StepEvent, ExecutionEvent.Id](c.value.stepEvents, c.pagingFirst, g, _.id)
              )
            )
        ),

        Field(
          name        = "stepQaState",
          fieldType   = OptionType(EnumTypeStepQaState),
          description = "Step QA state based on a combination of dataset QA states".some,
          resolve     = _.value.qaState
        ),

        Field(
          name        = "datasetEvents",
          fieldType   = DatasetEventConnectionType[F],
          description = "Dataset events associated with this step".some,
          arguments   = List(
            ArgumentPagingFirst,
            ArgumentPagingCursor
          ),
          resolve     = c =>
            unsafeSelectPageFuture[F, ExecutionEvent.Id, DatasetEvent](
              c.pagingExecutionEventId,
              d => Cursor.gid[ExecutionEvent.Id].reverseGet(d.id),
              g => Sync[F].delay(
                ResultPage.fromSeq[DatasetEvent, ExecutionEvent.Id](c.value.datasetEvents, c.pagingFirst, g, _.id)
              )
            )
        ),

        Field(
          name        = "datasets",
          fieldType   = DatasetSelectResult[F],
          description = "Datasets associated with this step".some,
          arguments   = List(
            ArgumentOptionOffsetDataset,
            ArgumentOptionLimit
          ),
          resolve     = c => {
            val where = WhereDatasetInput.matchStep(c.value.observationId, c.value.stepId)
            val off   = c.arg(ArgumentOptionOffsetDataset)
            val limit = c.resultSetLimit
            c.dataset(_.selectWhere(where, off, limit))
          }
        )

      )
    )

  def StepRecordEdgeType[F[_]: Dispatcher: Async: Logger, D](
    typePrefix:  String,
    dynamicType: OutputType[D]
  ): ObjectType[OdbCtx[F], Paging.Edge[StepRecord.Output[D]]] =
    Paging.EdgeType(
      s"${typePrefix}StepRecordEdge",
      "A step recorded by Observe and its cursor",
      StepRecordType[F, D](typePrefix, dynamicType)
    )

  def StepRecordConnectionType[F[_]: Dispatcher: Async: Logger, D](
    typePrefix:  String,
    dynamicType: OutputType[D]
  ): ObjectType[OdbCtx[F], Paging.Connection[StepRecord.Output[D]]] =
    Paging.ConnectionType(
      s"${typePrefix}StepRecordConnection",
      "Steps recorded by Observe in the current page",
      StepRecordType[F, D](typePrefix, dynamicType),
      StepRecordEdgeType[F, D](typePrefix, dynamicType)
    )

}

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.kernel.Sync
import cats.effect.std.Dispatcher
import cats.syntax.option._
import lucuma.core.model.ExecutionEvent
import lucuma.odb.api.model.{Step, StepRecord, Visit, VisitRecord}
import lucuma.odb.api.model.ExecutionEventModel.SequenceEvent
import lucuma.odb.api.repo.{OdbCtx, ResultPage}
import org.typelevel.log4cats.Logger
import sangria.schema._

object VisitRecordSchema {

  import context._
  import Paging._

  import ExecutionEventSchema.SequenceEventConnectionType
  import StepRecordSchema.StepRecordConnectionType
  import TimeSchema.{DurationType, InstantScalar}

  implicit val VisitIdType: ScalarType[Visit.Id] =
    ObjectIdSchema.uidType[Visit.Id]("VisitId")

  val ArgumentVisitId: Argument[Visit.Id] =
    Argument(
      name         = "visitId",
      argumentType = VisitIdType,
      description  = "Visit ID"
    )



  def VisitRecordType[F[_]: Dispatcher: Async: Logger, S, D](
    typePrefix:  String,
    staticType:  OutputType[S],
    dynamicType: OutputType[D]
  ): ObjectType[OdbCtx[F], VisitRecord.Output[S, D]] =

    ObjectType(
      name         = s"${typePrefix.capitalize}VisitRecord",
      description  = s"A ${typePrefix.capitalize} visit as recorded by Observe",
      fieldsFn     = () => fields(

        Field(
          name        = "id",
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
          fieldType   = DurationType,
          description = "Step duration".some,
          resolve     = _.value.duration
        ),

        Field(
          name        = "staticConfig",
          fieldType   = staticType,
          description = s"$typePrefix static instrument configuration".some,
          resolve     = _.value.staticConfig
        ),

        Field(
          name        = "steps",
          fieldType   = StepRecordConnectionType[F, D](typePrefix, dynamicType),
          description = s"$typePrefix recorded steps".some,
          arguments   = List(
            ArgumentPagingFirst,
            ArgumentPagingCursor
          ),
          resolve     = c =>
            unsafeSelectPageFuture[F, Step.Id, StepRecord.Output[D]](
              c.pagingStepId,
              s => Cursor.uid[Step.Id].reverseGet(s.stepId),
              u => Sync[F].delay(
                ResultPage.fromSeq[StepRecord.Output[D], Step.Id](c.value.steps, c.pagingFirst, u, _.stepId)
              )
            )
        ),

        Field(
          name        = "sequenceEvents",
          fieldType   = SequenceEventConnectionType[F],
          description = "Sequence events associated with this visit".some,
          arguments   = List(
            ArgumentPagingFirst,
            ArgumentPagingCursor
          ),
          resolve    = c =>
            unsafeSelectPageFuture[F, ExecutionEvent.Id, SequenceEvent](
              c.pagingExecutionEventId,
              e => Cursor.gid[ExecutionEvent.Id].reverseGet(e.id),
              g => Sync[F].delay(
                ResultPage.fromSeq[SequenceEvent, ExecutionEvent.Id](c.value.sequenceEvents, c.pagingFirst, g, _.id)
              )
            )

        )

      )
    )

  def VisitRecordEdgeType[F[_]: Dispatcher: Async: Logger, S, D](
    typePrefix:  String,
    staticType:  OutputType[S],
    dynamicType: OutputType[D]
  ): ObjectType[OdbCtx[F], Paging.Edge[VisitRecord.Output[S, D]]] =
    Paging.EdgeType(
      s"${typePrefix.capitalize}VisitRecordEdge",
      "A visit (and its cursor) as recorded by Observe",
      VisitRecordType[F, S, D](typePrefix, staticType, dynamicType)
    )

  def visitRecordConnectionType[F[_]: Dispatcher: Async: Logger, S, D](
    typePrefix:  String,
    staticType:  OutputType[S],
    dynamicType: OutputType[D]
  ): ObjectType[OdbCtx[F], Paging.Connection[VisitRecord.Output[S, D]]] =
    Paging.ConnectionType(
      s"${typePrefix.capitalize}VisitRecordConnection",
      "Visits as recorded by Observe",
      VisitRecordType[F, S, D](typePrefix, staticType, dynamicType),
      VisitRecordEdgeType[F, S, D](typePrefix, staticType, dynamicType)
    )
}

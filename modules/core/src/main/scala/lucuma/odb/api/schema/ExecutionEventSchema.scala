// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.model.ExecutionEvent
import lucuma.odb.api.model.ExecutionEventModel
import cats.effect.Async
import cats.effect.std.Dispatcher
import org.typelevel.log4cats.Logger
import sangria.schema._


object ExecutionEventSchema {

  import context._

  import TimeSchema._
  import ExecutionEventModel._
  import syntax.`enum`._

  implicit val ExecutionEventIdType: ScalarType[ExecutionEvent.Id] =
    ObjectIdSchema.idType[ExecutionEvent.Id](name = "ExecutionEventId")

  val ExecutionEventArgument: Argument[ExecutionEvent.Id] =
    Argument(
      name         = "executionEventId",
      argumentType = ExecutionEventIdType,
      description  = "Execution Event ID"
    )

  val OptionalExecutionEventArgument: Argument[Option[ExecutionEvent.Id]] =
    Argument(
      name         = "executionEventId",
      argumentType = OptionInputType(ExecutionEventIdType),
      description  = "Execution Event ID"
    )

  implicit val EnumTypeSequenceCommand: EnumType[SequenceCommandType] =
    EnumType.fromEnumerated(
      "SequenceCommand",
      "Sequence-level command"
    )

  implicit val EnumTypeStepStage: EnumType[StepStageType] =
    EnumType.fromEnumerated(
      "StepStage",
      "Execution stage or phase of an individual step"
    )

  implicit val EnumTypeDatasetStage: EnumType[DatasetStageType] =
    EnumType.fromEnumerated(
      "DatasetStage",
      "Execution stage or phase of an individual dataset"
    )

  def ExecutionEventType[F[_]: Dispatcher: Async: Logger]: InterfaceType[OdbCtx[F], ExecutionEventModel] =
    InterfaceType[OdbCtx[F], ExecutionEventModel](
      name         = "ExecutionEvent",
      description  = "Execution event (sequence, step, or dataset events)",
      fields[OdbCtx[F], ExecutionEventModel](

        Field(
          name        = "id",
          fieldType   = ExecutionEventIdType,
          description = Some("Event id"),
          resolve     = _.value.id
        ),

        Field(
          name        = "observation",
          fieldType   = ObservationSchema.ObservationType[F],
          description = Some("Observation whose execution produced this event"),
          resolve     = c => c.observation(_.unsafeSelect(c.value.observationId, includeDeleted = true))
        ),

        Field(
          name        = "generated",
          fieldType   = InstantScalar,
          description = Some("Time at which this event was generated, according to the caller (e.g., Observe)"),
          resolve     = _.value.generated
        ),

        Field(
          name        = "received",
          fieldType   = InstantScalar,
          description = Some("Time at which this event was received"),
          resolve     = _.value.received
        )

      )
    ).withPossibleTypes(() => List(
      PossibleObject[OdbCtx[F], ExecutionEventModel](SequenceEventType[F]),
      PossibleObject[OdbCtx[F], ExecutionEventModel](StepEventType[F]),
      PossibleObject[OdbCtx[F], ExecutionEventModel](DatasetEventType[F])
    ))

  def SequenceEventType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], SequenceEvent] =
    ObjectType[OdbCtx[F], SequenceEvent](
      name        = "SequenceEvent",
      description = "Sequence-level events",
      interfaces  = List(PossibleInterface.apply[OdbCtx[F], SequenceEvent](ExecutionEventType[F])),
      fields      = List[Field[OdbCtx[F], SequenceEvent]](

        Field(
          name        = "command",
          fieldType   = EnumTypeSequenceCommand,
          description = Some("Sequence command"),
          resolve     = _.value.command
        )

      )
    )

  def StepEventType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], StepEvent] =
    ObjectType[OdbCtx[F], StepEvent](
      name        = "StepEvent",
      description = "Step-level events",
      interfaces  = List(PossibleInterface.apply[OdbCtx[F], StepEvent](ExecutionEventType[F])),
      fields      = List[Field[OdbCtx[F], StepEvent]](

        Field(
          name        = "step",
          fieldType   = StepSchema.StepInterfaceType[F],
          description = Some("Step to which the event applies"),
          resolve     = c => c.step(_.unsafeSelectStep(c.value.stepId))
        ),

        Field(
          name        = "sequenceType",
          fieldType   = SequenceSchema.EnumTypeSequenceType,
          description = Some("Sequence type"),
          resolve     = _.value.sequenceType
        ),

        Field(
          name        = "stage",
          fieldType   = EnumTypeStepStage,
          description = Some("Step stage"),
          resolve     = _.value.stage
        )

      )
    )

  def DatasetEventType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], DatasetEvent] =
    ObjectType[OdbCtx[F], DatasetEvent](
      name        = "DatasetEvent",
      description = "Dataset-level events",
      interfaces  = List(PossibleInterface.apply[OdbCtx[F], DatasetEvent](ExecutionEventType[F])),
      fields      = List[Field[OdbCtx[F], DatasetEvent]](

        Field(
          name        = "step",
          fieldType   = StepSchema.StepInterfaceType[F],
          description = Some("Step from which the dataset comes"),
          resolve     = c => c.step(_.unsafeSelectStep(c.value.stepId))
        ),

        Field(
          name        = "filename",
          fieldType   = OptionType(DatasetSchema.DatasetFilenameScalar),
          description = Some("Dataset filename, when known"),
          resolve     = _.value.filename
        ),

        Field(
          name        = "stage",
          fieldType   = EnumTypeDatasetStage,
          description = Some("Dataset stage"),
          resolve     = _.value.stageType
        )
      )
    )

  def ExecutionEventEdgeType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], Paging.Edge[ExecutionEventModel]] =
    Paging.EdgeType(
      "ExecutionEventEdge",
      "An ExecutionEvent and its cursor",
      ExecutionEventType[F]
    )

  def ExecutionEventConnectionType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], Paging.Connection[ExecutionEventModel]] =
    Paging.ConnectionType(
      "ExecutionEventConnection",
      "ExecutionEvents in the current page",
      ExecutionEventType[F],
      ExecutionEventEdgeType[F]
    )
}

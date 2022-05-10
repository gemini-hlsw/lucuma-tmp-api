// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.model.ExecutionEvent
import lucuma.odb.api.model.ExecutionEventModel
import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.option._
import lucuma.odb.api.repo.OdbCtx
import org.typelevel.log4cats.Logger
import sangria.schema.{Field, _}


object ExecutionEventSchema {

  import context._

  import TimeSchema._
  import ExecutionEventModel._
  import ObservationSchema.ObservationIdType
  import StepSchema.StepIdType
  import VisitRecordSchema.VisitIdType
  import syntax.`enum`._

  implicit val ExecutionEventIdType: ScalarType[ExecutionEvent.Id] =
    ObjectIdSchema.gidType[ExecutionEvent.Id](name = "ExecutionEventId")

  val ExecutionEventArgument: Argument[ExecutionEvent.Id] =
    Argument(
      name         = "executionEventId",
      argumentType = ExecutionEventIdType,
      description  = "Execution Event ID"
    )

  val OptionalExecutionEventIdArgument: Argument[Option[ExecutionEvent.Id]] =
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
          name        = "visitId",
          fieldType   = VisitIdType,
          description = "Associated visit".some,
          resolve     = _.value.visitId
        ),

        Field(
          name        = "observation",
          fieldType   = ObservationSchema.ObservationType[F],
          description = Some("Observation whose execution produced this event"),
          resolve     = c => c.observation(_.unsafeSelect(c.value.observationId, includeDeleted = true))
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

  val SequenceEventLocationType: ObjectType[Any, SequenceEvent.Location] =
    ObjectType[Any, SequenceEvent.Location](
      name        = "SequenceEventLocation",
      description = "Sequence event location, i.e., to which observation the event refers",
      fields      = List[Field[Any, SequenceEvent.Location]](

        Field(
          name        = "observationId",
          fieldType   = ObservationIdType,
          description = "Observation containing the sequence".some,
          resolve     = _.value.observationId
        )

      )
    )

  val SequenceEventPayloadType: ObjectType[Any, SequenceEvent.Payload] =
    ObjectType[Any, SequenceEvent.Payload](
      name        = "SequenceEventPayload",
      description = "Sequence event data",
      fields      = List[Field[Any, SequenceEvent.Payload]](
        Field(
          name        = "command",
          fieldType   = EnumTypeSequenceCommand,
          description = Some("Sequence command"),
          resolve     = _.value.command
        )
      )
    )

  def SequenceEventType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], SequenceEvent] =
    ObjectType[OdbCtx[F], SequenceEvent](
      name        = "SequenceEvent",
      description = "Sequence-level events",
      interfaces  = List(PossibleInterface.apply[OdbCtx[F], SequenceEvent](ExecutionEventType[F])),
      fields      = List[Field[OdbCtx[F], SequenceEvent]](

        Field(
          name        = "location",
          fieldType   = SequenceEventLocationType,
          description = "Identifies the observation to which the event refers".some,
          resolve     = _.value.location
        ),

        Field(
          name        = "payload",
          fieldType   = SequenceEventPayloadType,
          description = "Sequence event data".some,
          resolve     = _.value.payload
        )

      )
    )

  val StepEventLocationType: ObjectType[Any, StepEvent.Location] =
    ObjectType[Any, StepEvent.Location](
      name        = "StepEventLocation",
      description = "Step event location, i.e., to which step the event refers",
      fields      = List[Field[Any, StepEvent.Location]](

        Field(
          name        = "observationId",
          fieldType   = ObservationIdType,
          description = "Observation containing the step".some,
          resolve     = _.value.observationId
        ),

        Field(
          name        = "stepId",
          fieldType   = StepIdType,
          description = "The step id itself".some,
          resolve     = _.value.stepId
        )

      )

    )

  val StepEventPayloadType: ObjectType[Any, StepEvent.Payload] =
    ObjectType[Any, StepEvent.Payload](
      name        = "StepEventPayload",
      description = "Step event data",
      fields      = List[Field[Any, StepEvent.Payload]](

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

  def StepEventType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], StepEvent] =
    ObjectType[OdbCtx[F], StepEvent](
      name        = "StepEvent",
      description = "Step-level events",
      interfaces  = List(PossibleInterface.apply[OdbCtx[F], StepEvent](ExecutionEventType[F])),
      fields      = List[Field[OdbCtx[F], StepEvent]](

        Field(
          name        = "location",
          fieldType   = StepEventLocationType,
          description = "Identifies the step to which the event refers".some,
          resolve     = _.value.location
        ),

        Field(
          name        = "payload",
          fieldType   = StepEventPayloadType,
          description = "Step event data".some,
          resolve     = _.value.payload
        )

      )
    )

  val DatasetEventPayloadType: ObjectType[Any, DatasetEvent.Payload] =
    ObjectType[Any, DatasetEvent.Payload](
      name        = "DatasetEventPayload",
      description = "Dataset event payload",
      fields      = List[Field[Any, DatasetEvent.Payload]](

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
          name        = "location",
          fieldType   = DatasetSchema.DatasetIdType,
          description = "Identifies the associated dataset".some,
          resolve     = _.value.datasetId
        ),

        Field(
          name        = "payload",
          fieldType   = DatasetEventPayloadType,
          description = "Dataset event payload".some,
          resolve     = _.value.payload
        )
      )
    )

  def SequenceEventEdgeType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], Paging.Edge[SequenceEvent]] =
    Paging.EdgeType(
      "SequenceEventEdge",
      "A sequence ExecutionEvent and its cursor",
      SequenceEventType[F]
    )

  def SequenceEventConnectionType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], Paging.Connection[SequenceEvent]] =
    Paging.ConnectionType(
      "SequenceEventConnection",
      "Sequence ExecutionEvents in the current page",
      SequenceEventType[F],
      SequenceEventEdgeType[F]
    )

  def StepEventEdgeType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], Paging.Edge[StepEvent]] =
    Paging.EdgeType(
      "StepEventEdge",
      "A step ExecutionEvent and its cursor",
      StepEventType[F]
    )

  def StepEventConnectionType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], Paging.Connection[StepEvent]] =
    Paging.ConnectionType(
      "StepEventConnection",
      "Step ExecutionEvents in the current page",
      StepEventType[F],
      StepEventEdgeType[F]
    )

  def DatasetEventEdgeType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], Paging.Edge[DatasetEvent]] =
    Paging.EdgeType(
      "DatasetEventEdge",
      "A dataset ExecutionEvent and its cursor",
      DatasetEventType[F]
    )

  def DatasetEventConnectionType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], Paging.Connection[DatasetEvent]] =
    Paging.ConnectionType(
      "DatasetEventConnection",
      "Dataset ExecutionEvents in the current page",
      DatasetEventType[F],
      DatasetEventEdgeType[F]
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

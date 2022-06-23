// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.model.ExecutionEvent
import lucuma.odb.api.model.{ExecutionEventModel, WhereDatasetEventInput, WhereExecutionEventInput, WhereSequenceEventInput, WhereStepEventInput}
import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.option._
import lucuma.odb.api.model.query.WhereOrderInput
import lucuma.odb.api.repo.OdbCtx
import org.typelevel.log4cats.Logger
import sangria.schema.{Field, _}


object ExecutionEventSchema {

  import context._

  import DatasetSchema.{DatasetIdType, InputObjectWhereOrderDatasetIndex}
  import TimeSchema._
  import ExecutionEventModel._
  import ObservationSchema.{ObservationIdType, InputObjectWhereOrderObservationId}
  import QuerySchema._
  import SequenceSchema.InputObjectWhereOrderSequenceType
  import StepSchema.{StepIdType, InputObjectWhereEqStepId}
  import VisitRecordSchema.{InputObjectWhereEqVisitId, VisitIdType}
  import syntax.`enum`._
  import syntax.inputtype._

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

  implicit val InputObjectWhereOrderExecutionEventId: InputObjectType[WhereOrderInput[ExecutionEvent.Id]] =
    inputObjectWhereOrder[ExecutionEvent.Id]("ExecutionEventId", ExecutionEventIdType)

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

  implicit val InputObjectWhereOrderStepStage: InputObjectType[WhereOrderInput[StepStageType]] =
    inputObjectWhereOrder("StepStage", EnumTypeStepStage)

  implicit val EnumTypeDatasetStage: EnumType[DatasetStageType] =
    EnumType.fromEnumerated(
      "DatasetStage",
      "Execution stage or phase of an individual dataset"
    )

  implicit val InputObjectWhereOrderDatasetStage: InputObjectType[WhereOrderInput[DatasetStageType]] =
    inputObjectWhereOrder("DatasetStage", EnumTypeDatasetStage)

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
      description = "Sequence event location, i.e., to which observation the event refers.",
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
      description = "Sequence-level events.  As commands are issued to execute a sequence, corresponding events are generated.",
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
          name        = "stepStage",
          fieldType   = EnumTypeStepStage,
          description = Some("Step execution stage"),
          resolve     = _.value.stepStage
        )
      )

    )

  def StepEventType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], StepEvent] =
    ObjectType[OdbCtx[F], StepEvent](
      name        = "StepEvent",
      description = "Step-level events.  The execution of a single step will generate multiple events.",
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
          description = "Step event data including the stage of execution through which it is passing.".some,
          resolve     = _.value.payload
        )

      )
    )

  val DatasetEventPayloadType: ObjectType[Any, DatasetEvent.Payload] =
    ObjectType[Any, DatasetEvent.Payload](
      name        = "DatasetEventPayload",
      description = "Dataset event payload.",
      fields      = List[Field[Any, DatasetEvent.Payload]](

        Field(
          name        = "filename",
          fieldType   = OptionType(DatasetSchema.DatasetFilenameScalar),
          description = Some("Dataset filename, when known"),
          resolve     = _.value.filename
        ),

        Field(
          name        = "datasetStage",
          fieldType   = EnumTypeDatasetStage,
          description = Some("Dataset execution stage"),
          resolve     = _.value.datasetStage
        )
      )
    )

  def DatasetEventType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], DatasetEvent] =
    ObjectType[OdbCtx[F], DatasetEvent](
      name        = "DatasetEvent",
      description = "Dataset-level events.  A single dataset will be associated with multiple events.",
      interfaces  = List(PossibleInterface.apply[OdbCtx[F], DatasetEvent](ExecutionEventType[F])),
      fields      = List[Field[OdbCtx[F], DatasetEvent]](

        Field(
          name        = "datasetId",
          fieldType   = DatasetIdType,
          description = "Identifies the associated dataset".some,
          resolve     = _.value.location
        ),

        Field(
          name        = "payload",
          fieldType   = DatasetEventPayloadType,
          description = "Dataset event payload, identifying the associated filename and stage of dataset execution".some,
          resolve     = _.value.payload
        )
      )
    )

  implicit val InputObjectWhereOrderSequenceCommandType: InputObjectType[WhereOrderInput[SequenceCommandType]] =
    inputObjectWhereOrder[SequenceCommandType]("SequenceCommand", EnumTypeSequenceCommand)

  implicit val InputObjectWhereSequenceEvent: InputObjectType[WhereSequenceEventInput] =
    InputObjectType[WhereSequenceEventInput](
      "WhereSequenceEvent",
      "SequenceEvent filter options.",
      List(
        InputObjectWhereOrderSequenceCommandType.optionField("command", "Matches the sequence command type")
      )
    )

  implicit val InputObjectWhereStepEvent: InputObjectType[WhereStepEventInput] =
    InputObjectType[WhereStepEventInput](
      "WhereStepEvent",
      "StepEvent filter options.",
      List(
        InputObjectWhereEqStepId.optionField("stepId", "Matches on the step id."),
        InputObjectWhereOrderSequenceType.optionField("sequenceType", "Matches on the sequence type"),
        InputObjectWhereOrderStepStage.optionField("stage", "Matches on the step stage")
      )
    )

  implicit val InputObjectWhereDatasetEvent: InputObjectType[WhereDatasetEventInput] =
    InputObjectType[WhereDatasetEventInput](
      "WhereDatasetEvent",
      "DatasetEvent filter options.",
      List(
        InputObjectWhereEqStepId.optionField("stepId", "Matches on the step id."),
        InputObjectWhereOrderDatasetIndex.optionField("index", "Matches on the dataset index within the step."),
        InputObjectWhereOrderDatasetStage.optionField("stage", "Matches on the dataset stage."),
        InputObjectWhereOptionString.optionField("filename", "Matches on the dataset filename.")
      )
    )

  implicit val InputObjectWhereExecutionEvent: InputObjectType[WhereExecutionEventInput] =
    InputObjectType[WhereExecutionEventInput](
      "WhereExecutionEvent",
      "ExecutionEvent filter options.",
      () =>
      combinatorFields(InputObjectWhereExecutionEvent, "execution event") :::
        List(
          InputObjectWhereOrderExecutionEventId.optionField("id", "Matches on the execution event id"),
          InputObjectWhereEqVisitId.optionField("visitId", "Matches on the visit id"),
          InputObjectWhereOrderObservationId.optionField("observationId", "Matches on observation id"),
          InputObjectWhereOrderInstant.optionField("received", "Matches on event reception time"),
          InputObjectWhereSequenceEvent.optionField("sequenceEvent", "Matches sequence events only"),
          InputObjectWhereStepEvent.optionField("stepEvent", "Matches step events only"),
          InputObjectWhereDatasetEvent.optionField("datasetEvent", "Matches dataset events only")
        )
    )

}

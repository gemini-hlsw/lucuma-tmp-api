// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.model.ExecutionEvent
import lucuma.odb.api.model.ExecutionEventModel
import lucuma.odb.api.repo.OdbRepo

import cats.MonadError
import cats.effect.std.Dispatcher
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

  def ExecutionEventType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): InterfaceType[OdbRepo[F], ExecutionEventModel] =
    InterfaceType[OdbRepo[F], ExecutionEventModel](
      name         = "ExecutionEvent",
      description  = "Execution event (sequence, step, or dataset events)",
      fields[OdbRepo[F], ExecutionEventModel](

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
      PossibleObject[OdbRepo[F], ExecutionEventModel](SequenceEventType[F]),
      PossibleObject[OdbRepo[F], ExecutionEventModel](StepEventType[F]),
      PossibleObject[OdbRepo[F], ExecutionEventModel](DatasetEventType[F])
    ))

  def SequenceEventType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], SequenceEvent] =
    ObjectType[OdbRepo[F], SequenceEvent](
      name        = "SequenceEvent",
      description = "Sequence-level events",
      interfaces  = List(PossibleInterface.apply[OdbRepo[F], SequenceEvent](ExecutionEventType[F])),
      fields      = List[Field[OdbRepo[F], SequenceEvent]](

        Field(
          name        = "command",
          fieldType   = EnumTypeSequenceCommand,
          description = Some("Sequence command"),
          resolve     = _.value.command
        )

      )
    )

  def StepEventType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], StepEvent] =
    ObjectType[OdbRepo[F], StepEvent](
      name        = "StepEvent",
      description = "Step-level events",
      interfaces  = List(PossibleInterface.apply[OdbRepo[F], StepEvent](ExecutionEventType[F])),
      fields      = List[Field[OdbRepo[F], StepEvent]](

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

  def DatasetEventType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], DatasetEvent] =
    ObjectType[OdbRepo[F], DatasetEvent](
      name        = "DatasetEvent",
      description = "Dataset-level events",
      interfaces  = List(PossibleInterface.apply[OdbRepo[F], DatasetEvent](ExecutionEventType[F])),
      fields      = List[Field[OdbRepo[F], DatasetEvent]](

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

  def ExecutionEventEdgeType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], Paging.Edge[ExecutionEventModel]] =
    Paging.EdgeType(
      "ExecutionEventEdge",
      "An ExecutionEvent and its cursor",
      ExecutionEventType[F]
    )

  def ExecutionEventConnectionType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], Paging.Connection[ExecutionEventModel]] =
    Paging.ConnectionType(
      "ExecutionEventConnection",
      "ExecutionEvents in the current page",
      ExecutionEventType[F],
      ExecutionEventEdgeType[F]
    )
}

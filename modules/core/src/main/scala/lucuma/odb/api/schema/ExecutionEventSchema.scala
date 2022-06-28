// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.Applicative
import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import io.circe.Decoder
import lucuma.core.model.ExecutionEvent
import lucuma.odb.api.model.{ExecutionEventModel, InputValidator, Step, StepRecord, WhereDatasetEventInput, WhereExecutionEventInput, WhereSequenceEventInput, WhereStepEventInput, Visit, VisitRecord, VisitRecords}
import lucuma.odb.api.model.GmosModel.{CreateNorthDynamic, CreateNorthStatic, CreateSouthDynamic, CreateSouthStatic, NorthDynamic, NorthStatic, SouthDynamic, SouthStatic}
import lucuma.odb.api.model.query.{SizeLimitedResult, WhereOrderInput}
import lucuma.odb.api.repo.OdbCtx
import monocle.Prism
import org.typelevel.log4cats.Logger
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema.{Field, _}

import scala.collection.immutable.ListMap

object ExecutionEventSchema {

  import context._

  import DatasetSchema.{DatasetFilenameScalar, DatasetIdType, InputObjectDatasetId, InputObjectWhereOrderDatasetIndex}
  import TimeSchema._
  import ExecutionEventModel._
  import ObservationSchema.{ObservationIdType, InputObjectWhereOrderObservationId}
  import QuerySchema._
  import SequenceSchema.{EnumTypeSequenceType, InputObjectWhereOrderSequenceType}
  import StepMutation.InputObjectTypeCreateStepConfig
  import StepSchema.{ArgumentStepId, StepIdType, InputObjectWhereEqStepId}
  import StepRecordSchema.StepRecordType
  import VisitRecordSchema.{ArgumentVisitId, InputObjectWhereEqVisitId, VisitIdType, VisitRecordType}
  import syntax.`enum`._
  import syntax.inputobjecttype._
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

  implicit val ArgumentOptionWhereExecutionEvent: Argument[Option[WhereExecutionEventInput]] =
    Argument(
      name         = "WHERE",
      argumentType = OptionInputType(InputObjectWhereExecutionEvent),
      description  = "Filters the selection of execution events"
    )

  implicit val ArgumentOptionOffsetExecutionEvent: Argument[Option[ExecutionEvent.Id]] =
    Argument(
      name         = "OFFSET",
      argumentType = OptionInputType(ExecutionEventIdType),
      description  = "Starts the result set at (or after if not existent) the given execution event id."
    )

  implicit def ExecutionEventSelectResult[F[_]: Dispatcher: Async: Logger]: ObjectType[Any, SizeLimitedResult[ExecutionEventModel]] =
    SelectResultType[ExecutionEventModel]("ExecutionEvent", ExecutionEventType[F])

  def executionEvents[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "executionEvents",
      fieldType   = ExecutionEventSelectResult[F],
      description = "Selects the first `LIMIT` matching execution events based on the provided `WHERE` parameter, if any.".some,
      arguments   = List(
        ArgumentOptionWhereExecutionEvent,
        ArgumentOptionOffsetExecutionEvent,
        ArgumentOptionLimit
      ),
      resolve     = c => {
        val where = c.arg(ArgumentOptionWhereExecutionEvent).getOrElse(WhereExecutionEventInput.MatchAll)
        val off   = c.arg(ArgumentOptionOffsetExecutionEvent)
        val limit = c.resultSetLimit
        c.executionEvent(_.selectWhere(where, off, limit))
      }
    )

  def queryFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      executionEvents
    )

  def InputObjectVisitRecordInput[SI](
    typePrefix:  String,
    staticInput: InputObjectType[SI]
  ): InputObjectType[VisitRecord.Input[SI]] =
    InputObjectType[VisitRecord.Input[SI]](
      s"Record${typePrefix.capitalize}VisitInput",
      s"Input parameters for creating a new ${typePrefix.capitalize} VisitRecord",
      List(
        InputField("observationId", ObservationIdType),
        InputField("static", staticInput)
      )
    )

  def RecordVisitResult[F[_]: Dispatcher: Async: Logger, S, D](
    typePrefix:  String,
    staticType:  OutputType[S],
    dynamicType: OutputType[D]
  ): ObjectType[OdbCtx[F], VisitRecord.RecordResult[S, D]] =
    ObjectType(
      name        = s"Record${typePrefix.capitalize}VisitResult",
      description = s"The result of recording a ${typePrefix.capitalize} visit.",
      fieldsFn    = () => fields(

        Field(
          name        = "visitRecord",
          description = "The newly added visit record itself.".some,
          fieldType   = VisitRecordType[F, S, D](typePrefix, staticType, dynamicType),
          resolve     = _.value.visitRecord
        )

      )
    )

  def recordVisit[F[_]: Dispatcher: Async: Logger, SI: Decoder, S, D](
    typePrefix:  String,
    staticInput: InputObjectType[SI],
    staticType:  OutputType[S],
    dynamicType: OutputType[D],
    prism:       Prism[VisitRecords, ListMap[Visit.Id, VisitRecord[S, D]]],
    testing:     Boolean
  )(implicit ev: InputValidator[SI, S]): Field[OdbCtx[F], Unit] = {

    val vriArg =
      InputObjectVisitRecordInput[SI](typePrefix, staticInput).argument(
        "input",
        "VisitRecord creation parameters"
      )

    val args = vriArg :: (if (testing) List(ArgumentVisitId) else Nil)

    Field(
      name        = s"record${typePrefix.capitalize}Visit",
      fieldType   = RecordVisitResult[F, S, D](typePrefix, staticType, dynamicType),
      description = "Record a new visit".some,
      arguments   = args,
      resolve     = c => {
        val params      = c.arg[VisitRecord.Input[SI]]("input")
        val insertVisit =
          for {
            vid  <- if (testing) Applicative[F].pure(c.arg(ArgumentVisitId)) else Visit.Id.random[F]
            rec  <- c.ctx.odbRepo.executionEvent.insertVisit[SI, S, D](vid, params, prism)
          } yield VisitRecord.RecordResult(rec)

        c.unsafeToFuture(insertVisit)
      }
    )
  }

  def recordGmosNorthVisit[F[_]: Dispatcher: Async: Logger](
    testing: Boolean
  ): Field[OdbCtx[F], Unit] =
    recordVisit[F, CreateNorthStatic, NorthStatic, NorthDynamic](
      "GmosNorth",
      GmosSchema.InputObjectGmosNorthStaticInput,
      GmosSchema.GmosNorthStaticConfigType,
      GmosSchema.GmosNorthDynamicType,
      VisitRecords.gmosNorthVisits,
      testing
    )

  def recordGmosSouthVisit[F[_]: Dispatcher: Async: Logger](
    testing: Boolean
  ): Field[OdbCtx[F], Unit] =
    recordVisit[F, CreateSouthStatic, SouthStatic, SouthDynamic](
      "GmosSouth",
      GmosSchema.InputObjectGmosSouthStaticInput,
      GmosSchema.GmosSouthStaticConfigType,
      GmosSchema.GmosSouthDynamicType,
      VisitRecords.gmosSouthVisits,
      testing
    )

  def InputObjectStepRecordInput[DI](
    typePrefix: String,
    stepType:   InputType[DI]
  ): InputObjectType[StepRecord.Input[DI]] =
    InputObjectType[StepRecord.Input[DI]](
      s"Record${typePrefix.capitalize}StepInput",
      s"Input parameters for creating a new ${typePrefix.capitalize} StepRecord",
      List(
        InputField("observationId", ObservationIdType),
        InputField("visitId",       VisitIdType),
        InputField("stepConfig",    InputObjectTypeCreateStepConfig(typePrefix, stepType))
      )
    )

  def RecordStepResult[F[_]: Dispatcher: Async: Logger, D](
    typePrefix:  String,
    dynamicType: OutputType[D]
  ): ObjectType[OdbCtx[F], StepRecord.RecordResult[D]] =
    ObjectType(
      name        = s"Record${typePrefix.capitalize}StepResult",
      description = s"The result of recording a ${typePrefix.capitalize} step.",
      fieldsFn    = () => fields(

        Field(
          name        = "stepRecord",
          description = "The newly added step record itself.".some,
          fieldType   = StepRecordType[F, D](typePrefix, dynamicType),
          resolve     = _.value.stepRecord
        )

      )
    )

  def recordStep[F[_]: Dispatcher: Async: Logger, DI: Decoder, S, D](
    typePrefix:   String,
    dynamicInput: InputObjectType[DI],
    dynamicType:  OutputType[D],
    prism:        Prism[VisitRecords, ListMap[Visit.Id, VisitRecord[S, D]]],
    testing:      Boolean
  )(implicit ev: InputValidator[DI, D]): Field[OdbCtx[F], Unit] = {

    val sriArg = InputObjectStepRecordInput[DI](typePrefix, dynamicInput).argument(
      "input",
      s"${typePrefix.capitalize} step configuration parameters"
    )

    val args = sriArg :: (if (testing) List(ArgumentStepId) else Nil)

    Field(
      name        = s"record${typePrefix.capitalize}Step",
      fieldType   = RecordStepResult[F, D](typePrefix, dynamicType),
      description = "Record a new step".some,
      arguments   = args,
      resolve     = c => {
        val params     = c.arg[StepRecord.Input[DI]]("input")
        val insertStep =
          for {
            sid <- if (testing) Applicative[F].pure(c.arg(ArgumentStepId)) else Step.Id.random[F]
            rec <- c.ctx.odbRepo.executionEvent.insertStep[DI, S, D](sid, params, prism)
          } yield StepRecord.RecordResult(rec)

        c.unsafeToFuture(insertStep)
      }
    )
  }

  def recordGmosNorthStep[F[_]: Dispatcher: Async: Logger](
    testing: Boolean
  ): Field[OdbCtx[F], Unit] =
    recordStep[F, CreateNorthDynamic, NorthStatic, NorthDynamic](
      "GmosNorth",
      GmosSchema.InputObjectTypeGmosNorthDynamic,
      GmosSchema.GmosNorthDynamicType,
      VisitRecords.gmosNorthVisits,
      testing
    )

  def recordGmosSouthStep[F[_]: Dispatcher: Async: Logger](
    testing: Boolean
  ): Field[OdbCtx[F], Unit] =
    recordStep[F, CreateSouthDynamic, SouthStatic, SouthDynamic](
      "GmosSouth",
      GmosSchema.InputObjectTypeGmosSouthDynamic,
      GmosSchema.GmosSouthDynamicType,
      VisitRecords.gmosSouthVisits,
      testing
    )

  // SequenceEvent ------------------------------------------------------------

  implicit val InputObjectTypeSequenceEventLocation: InputObjectType[SequenceEvent.Location] =
    deriveInputObjectType[SequenceEvent.Location](
      InputObjectTypeName("SequenceEventLocationInput"),
      InputObjectTypeDescription("SequenceEvent location parameters")
    )

  implicit val InputObjectTypeSequenceEventPayload: InputObjectType[SequenceEvent.Payload] =
    deriveInputObjectType[SequenceEvent.Payload](
      InputObjectTypeName("SequenceEventPayloadInput"),
      InputObjectTypeDescription("SequenceEvent payload creation parameters")
    )

  val InputObjectTypeSequenceEventAdd: InputObjectType[SequenceEvent.Add] =
    deriveInputObjectType[SequenceEvent.Add](
      InputObjectTypeName("AddSequenceEventInput"),
      InputObjectTypeDescription("SequenceEvent creation parameters")
    )

  def AddSequenceEventResultType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], SequenceEvent.Result] =
    ObjectType[OdbCtx[F], SequenceEvent.Result](
      name        = "AddSequenceEventResult",
      description = "The result of adding a sequence event.",
      fields      = List[Field[OdbCtx[F], SequenceEvent.Result]](

        Field(
          name        = "event",
          description = "The new sequence event that was added.".some,
          fieldType   = SequenceEventType[F],
          resolve     = _.value.event
        )

      )
    )

  val ArgumentSequenceEventAdd: Argument[SequenceEvent.Add] =
    InputObjectTypeSequenceEventAdd.argument(
      name        = "input",
      description =
      """Describes the sequence event to add.  All events are associated with a
        |particular visit (see 'record{InstrumentName}Visit').  Sequence events
        |are further associated with an observation (i.e., its 'location').
        |Each sequence event 'payload' identifies the sequence command that
        |acted upon the sequence.
      """.stripMargin
    )

  def addSequenceEvent[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "addSequenceEvent",
      description =
      """Adds a sequence event associated with the given visit. Multiple events
        |will be produced during the execution of a sequence as it is started,
        |paused, continued, etc.
      """.stripMargin.some,
      fieldType   = AddSequenceEventResultType[F],
      arguments   = List(ArgumentSequenceEventAdd),
      resolve     = c => c.executionEvent(_.insertSequenceEvent(c.arg(ArgumentSequenceEventAdd)))
    )


  // StepEvent ----------------------------------------------------------------

  implicit val InputObjectTypeStepEventLocation: InputObjectType[StepEvent.Location] =
    deriveInputObjectType[StepEvent.Location](
      InputObjectTypeName("StepEventLocationInput"),
      InputObjectTypeDescription("StepEvent location parameters")
    )

  implicit val InputObjectTypeStepEventPayload: InputObjectType[StepEvent.Payload] =
    deriveInputObjectType[StepEvent.Payload](
      InputObjectTypeName("StepEventPayloadInput"),
      InputObjectTypeDescription("StepEvent payload parameters")
    )

  val InputObjectTypeStepEventAdd: InputObjectType[StepEvent.Add] =
    deriveInputObjectType[StepEvent.Add](
      InputObjectTypeName("AddStepEventInput"),
      InputObjectTypeDescription("StepEvent creation parameters")
    )

  val ArgumentStepEventAdd: Argument[StepEvent.Add] =
    InputObjectTypeStepEventAdd.argument(
      name        = "input",
      description =
      """Describes the step event to add.  All events are associated with a
        |particular visit (see 'record{InstrumentName}Visit'.  Step events are
        |further associated with an observation and step (i.e., its 'location').
        |(See also 'record{InstrumentName}Step'.) Each step event 'payload'
        |identifies the sequence type (acquisition or science) and the stage
        |through which the step execution is passing.
      """.stripMargin
    )

  def AddStepEventResultType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], StepEvent.Result] =
    ObjectType[OdbCtx[F], StepEvent.Result](
      name        = "AddStepEventResult",
      description = "The result of adding a step event.",
      fields      = List[Field[OdbCtx[F], StepEvent.Result]](

        Field(
          name        = "event",
          description = "The new step event that was added.".some,
          fieldType   = StepEventType[F],
          resolve     = _.value.event
        )

      )
    )

  def addStepEvent[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "addStepEvent",
      description =
      """Adds a new step event associated with the given visit. Multiple events
        |will be produced during the execution of a single step as it
        |transitions through configure and observe stages.
      """.stripMargin.some,
      fieldType   = AddStepEventResultType[F],
      arguments   = List(ArgumentStepEventAdd),
      resolve     = c => c.executionEvent(_.insertStepEvent(c.arg(ArgumentStepEventAdd)))
    )


  // DatasetEvent -------------------------------------------------------------

  implicit val InputObjectTypeDatasetEventPayload: InputObjectType[DatasetEvent.Payload] =
    deriveInputObjectType[DatasetEvent.Payload](
      InputObjectTypeName("DatasetEventPayloadInput"),
      InputObjectTypeDescription("DatasetEvent payload parameters")
    )

  val InputObjectTypeDatasetEventAdd: InputObjectType[DatasetEvent.Add] =
    deriveInputObjectType[DatasetEvent.Add](
      InputObjectTypeName("AddDatasetEventInput"),
      InputObjectTypeDescription("DatasetEvent creation parameters")
    )

  val ArgumentDatasetEventAdd: Argument[DatasetEvent.Add] =
    InputObjectTypeDatasetEventAdd.argument(
      name        = "input",
      description =
      """Describes the dataset event to add.  All events are associated with a
        |particular visit (see 'record{InstrumentName}Visit').  Dataset events
        |are further associated with an |observation, step and index because a
        |step may produce multiple datasets.  The three form its 'location'.
        |(See also 'record{InstrumentName}Step'.) Each dataset event 'payload'
        |identifies the stage (observe, readout, or write) and possibly the
        |dataset filename.
      """.stripMargin
    )

  def AddDatasetEventResultType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], DatasetEvent.Result] =
    ObjectType[OdbCtx[F], DatasetEvent.Result](
      name        = "AddDatasetEventResult",
      description = "The result of adding a dataset event.",
      fields      = List[Field[OdbCtx[F], DatasetEvent.Result]](

        Field(
          name        = "event",
          description = "The new dataset event that was added.".some,
          fieldType   = DatasetEventType[F],
          resolve     = _.value.event
        )

      )
    )


  def addDatasetEvent[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "addDatasetEvent",
      description =
      """Adds a new dataset event associated with the given visit.  The
        |generation of a single dataset will produce multiple events as it
        |transitions through the observe, readout and write stages.
      """.stripMargin.some,
      fieldType   = AddDatasetEventResultType[F],
      arguments   = List(ArgumentDatasetEventAdd),
      resolve     = c => c.executionEvent(_.insertDatasetEvent(c.arg(ArgumentDatasetEventAdd)))
    )

  // --------------------------------------------------------------------------

  def mutationFields[F[_]: Dispatcher: Async: Logger](
    testing: Boolean
  ): List[Field[OdbCtx[F], Unit]] =
    List(
      addSequenceEvent,
      addStepEvent,
      addDatasetEvent,
      recordGmosNorthVisit(testing),
      recordGmosNorthStep(testing),
      recordGmosSouthVisit(testing),
      recordGmosSouthStep(testing)
    )

}

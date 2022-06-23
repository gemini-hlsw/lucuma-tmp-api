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
import lucuma.odb.api.model.{ExecutionEventModel, InputValidator, Step, StepRecord, Visit, VisitRecord, VisitRecords}
import lucuma.odb.api.model.GmosModel.{CreateNorthDynamic, CreateNorthStatic, CreateSouthDynamic, CreateSouthStatic, NorthDynamic, NorthStatic, SouthDynamic, SouthStatic}
import lucuma.odb.api.repo.OdbCtx
import monocle.Prism
import org.typelevel.log4cats.Logger
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._

import scala.collection.immutable.ListMap


trait ExecutionEventMutation {

  import DatasetSchema.{InputObjectDatasetId, DatasetFilenameScalar}
  import ExecutionEventSchema.{DatasetEventType, EnumTypeDatasetStage, EnumTypeSequenceCommand, EnumTypeStepStage, SequenceEventType, StepEventType}
  import ExecutionEventModel.{DatasetEvent, SequenceEvent, StepEvent}
  import ObservationSchema.ObservationIdType
  import SequenceSchema.EnumTypeSequenceType
  import StepMutation.InputObjectTypeCreateStepConfig
  import StepSchema.{ArgumentStepId, StepIdType}
  import StepRecordSchema.StepRecordType
  import VisitRecordSchema.{ArgumentVisitId, VisitIdType, VisitRecordType}
  import syntax.inputobjecttype._
  import context._


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

  def allFields[F[_]: Dispatcher: Async: Logger](
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

object ExecutionEventMutation extends ExecutionEventMutation
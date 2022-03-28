// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

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

  import DatasetSchema.DatasetFilenameScalar
  import ExecutionEventSchema.{DatasetEventType, EnumTypeDatasetStage, EnumTypeSequenceCommand, EnumTypeStepStage, SequenceEventType, StepEventType}
  import ExecutionEventModel.{DatasetEvent, SequenceEvent, StepEvent}
  import ObservationSchema.ObservationIdType
  import RefinedSchema.InputObjectPosInt
  import SequenceSchema.EnumTypeSequenceType
  import StepMutation.InputObjectTypeCreateStepConfig
  import StepSchema.StepIdType
  import StepRecordSchema.StepRecordType
  import VisitRecordSchema.{VisitIdType, VisitRecordType}
  import syntax.inputobjecttype._
  import context._


  def InputObjectVisitRecordInput[SI](
    typePrefix:  String,
    staticInput: InputObjectType[SI]
  ): InputObjectType[VisitRecord.Input[SI]] =
    InputObjectType[VisitRecord.Input[SI]](
      s"${typePrefix.capitalize}VisitRecordInput",
      s"Input parameters for creating a new ${typePrefix.capitalize} VisitRecord",
      List(
        InputField("observationId", ObservationIdType),
        InputField("static", staticInput)
      )
    )

  def recordVisit[F[_]: Dispatcher: Async: Logger, SI: Decoder, S, D](
    typePrefix:  String,
    staticInput: InputObjectType[SI],
    staticType:  OutputType[S],
    dynamicType: OutputType[D],
    prism:       Prism[VisitRecords, ListMap[Visit.Id, VisitRecord[S, D]]]
  )(implicit ev: InputValidator[SI, S]): Field[OdbCtx[F], Unit] =

    Field(
      name        = s"record${typePrefix.capitalize}Visit",
      fieldType   = VisitRecordType[F, S, D](typePrefix, staticType, dynamicType),
      description = "Record a new visit".some,
      arguments   = List(
        InputObjectVisitRecordInput[SI](typePrefix, staticInput).argument(
          "input",
          "VisitRecord creation parameters"
        )
      ),
      resolve     = c => {
        val params      = c.arg[VisitRecord.Input[SI]]("input")
        val insertVisit =
          for {
            vid  <- Visit.Id.random[F]
            rec  <- c.ctx.odbRepo.executionEvent.insertVisit[SI, S, D](vid, params, prism)
          } yield rec

        c.unsafeToFuture(insertVisit)
      }
    )

  def recordGmosNorthVisit[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    recordVisit[F, CreateNorthStatic, NorthStatic, NorthDynamic](
      "GmosNorth",
      GmosSchema.InputObjectGmosNorthStaticInput,
      GmosSchema.GmosNorthStaticConfigType,
      GmosSchema.GmosNorthDynamicType,
      VisitRecords.gmosNorthVisits
    )

  def recordGmosSouthVisit[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    recordVisit[F, CreateSouthStatic, SouthStatic, SouthDynamic](
      "GmosSouth",
      GmosSchema.InputObjectGmosSouthStaticInput,
      GmosSchema.GmosSouthStaticConfigType,
      GmosSchema.GmosSouthDynamicType,
      VisitRecords.gmosSouthVisits
    )

  def InputObjectStepRecordInput[DI](
    typePrefix: String,
    stepType:   InputType[DI]
  ): InputObjectType[StepRecord.Input[DI]] =
    InputObjectType[StepRecord.Input[DI]](
      s"${typePrefix.capitalize}StepRecordInput",
      s"Input parameters for creating a new ${typePrefix.capitalize} StepRecord",
      List(
        InputField("observationId", ObservationIdType),
        InputField("visitId",       VisitIdType),
        InputField("stepConfig",    InputObjectTypeCreateStepConfig(typePrefix, stepType))
      )
    )

  def recordStep[F[_]: Dispatcher: Async: Logger, DI: Decoder, S, D](
    typePrefix:   String,
    dynamicInput: InputObjectType[DI],
    dynamicType:  OutputType[D],
    prism:        Prism[VisitRecords, ListMap[Visit.Id, VisitRecord[S, D]]]
  )(implicit ev: InputValidator[DI, D]): Field[OdbCtx[F], Unit] =

    Field(
      name        = s"record${typePrefix.capitalize}Step",
      fieldType   = StepRecordType[F, D](typePrefix, dynamicType),
      description = "Record a new step".some,
      arguments   = List(
        InputObjectStepRecordInput[DI](
          typePrefix,
          dynamicInput
        ).argument(
          "input",
          s"${typePrefix.capitalize} step configuration parameters"
        )
      ),
      resolve     = c => {
        val params     = c.arg[StepRecord.Input[DI]]("input")
        val insertStep =
          for {
            sid <- Step.Id.random[F]
            rec <- c.ctx.odbRepo.executionEvent.insertStep[DI, S, D](sid, params, prism)
          } yield rec

        c.unsafeToFuture(insertStep)
      }
    )

  def recordGmosNorthStep[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    recordStep[F, CreateNorthDynamic, NorthStatic, NorthDynamic](
      "GmosNorth",
      GmosSchema.InputObjectTypeGmosNorthDynamic,
      GmosSchema.GmosNorthDynamicType,
      VisitRecords.gmosNorthVisits
    )

  def recordGmosSouthStep[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    recordStep[F, CreateSouthDynamic, SouthStatic, SouthDynamic](
      "GmosSouth",
      GmosSchema.InputObjectTypeGmosSouthDynamic,
      GmosSchema.GmosSouthDynamicType,
      VisitRecords.gmosSouthVisits
    )

  // SequenceEvent ------------------------------------------------------------

  val InputObjectTypeSequenceEventAdd: InputObjectType[SequenceEvent.Add] =
    deriveInputObjectType[SequenceEvent.Add](
      InputObjectTypeName("AddSequenceEventInput"),
      InputObjectTypeDescription("SequenceEvent creation parameters")
    )

  val ArgumentSequenceEventAdd: Argument[SequenceEvent.Add] =
    InputObjectTypeSequenceEventAdd.argument(
      "input",
      "Sequence event description"
    )

  def addSequenceEvent[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name      = "addSequenceEvent",
      fieldType = SequenceEventType[F],
      arguments = List(ArgumentSequenceEventAdd),
      resolve   = c => c.executionEvent(_.insertSequenceEvent(c.arg(ArgumentSequenceEventAdd)))
    )


  // StepEvent ----------------------------------------------------------------

  val InputObjectTypeStepEventAdd: InputObjectType[StepEvent.Add] =
    deriveInputObjectType[StepEvent.Add](
      InputObjectTypeName("AddStepEventInput"),
      InputObjectTypeDescription("StepEvent creation parameters")
    )

  val ArgumentStepEventAdd: Argument[StepEvent.Add] =
    InputObjectTypeStepEventAdd.argument(
      "input",
      "Step event description"
    )

  def addStepEvent[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name      = "addStepEvent",
      fieldType = StepEventType[F],
      arguments = List(ArgumentStepEventAdd),
      resolve   = c => c.executionEvent(_.insertStepEvent(c.arg(ArgumentStepEventAdd)))
    )


  // DatasetEvent -------------------------------------------------------------

  val InputObjectTypeDatasetEventAdd: InputObjectType[DatasetEvent.Add] =
    deriveInputObjectType[DatasetEvent.Add](
      InputObjectTypeName("AddDatasetEventInput"),
      InputObjectTypeDescription("DatasetEvent creation parameters")
    )

  val ArgumentDatasetEventAdd: Argument[DatasetEvent.Add] =
    InputObjectTypeDatasetEventAdd.argument(
      "input",
      "Dataset event description"
    )

  def addDatasetEvent[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name      = "addDatasetEvent",
      fieldType = DatasetEventType[F],
      arguments = List(ArgumentDatasetEventAdd),
      resolve   = c => c.executionEvent(_.insertDatasetEvent(c.arg(ArgumentDatasetEventAdd)))
    )

  // --------------------------------------------------------------------------

  def allFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      addSequenceEvent,
      addStepEvent,
      addDatasetEvent,
      recordGmosNorthVisit,
      recordGmosNorthStep,
      recordGmosSouthVisit,
      recordGmosSouthStep
    )

}

object ExecutionEventMutation extends ExecutionEventMutation
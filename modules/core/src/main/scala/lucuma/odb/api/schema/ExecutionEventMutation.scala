// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.ExecutionEventModel
import lucuma.odb.api.repo.OdbRepo
import lucuma.odb.api.schema.ExecutionEventSchema.{DatasetEventType, SequenceEventType, StepEventType}

import cats.MonadError
import cats.effect.std.Dispatcher
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._


trait ExecutionEventMutation {

  import DatasetSchema.DatasetFilenameScalar
  import ExecutionEventSchema.{ExecutionEventIdType, EnumTypeDatasetStage, EnumTypeSequenceCommand, EnumTypeStepStage}
  import ExecutionEventModel.{DatasetEvent, SequenceEvent, StepEvent}
  import ObservationSchema.ObservationIdType
  import RefinedSchema.InputObjectPosInt
  import SequenceSchema.EnumTypeSequenceType
  import StepSchema.StepIdType
  import TimeSchema.InstantScalar
  import syntax.inputobjecttype._
  import context._

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

  def addSequenceEvent[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
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

  def addStepEvent[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
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

  def addDatasetEvent[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "addDatasetEvent",
      fieldType = DatasetEventType[F],
      arguments = List(ArgumentDatasetEventAdd),
      resolve   = c => c.executionEvent(_.insertDatasetEvent(c.arg(ArgumentDatasetEventAdd)))
    )

  // --------------------------------------------------------------------------

  def allFields[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): List[Field[OdbRepo[F], Unit]] =
    List(
      addSequenceEvent,
      addStepEvent,
      addDatasetEvent
    )

}

object ExecutionEventMutation extends ExecutionEventMutation {

}
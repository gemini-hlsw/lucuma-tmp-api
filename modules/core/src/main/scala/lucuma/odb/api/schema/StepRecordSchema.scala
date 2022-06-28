// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{StepQaState, StepRecord}
import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.all._
import lucuma.odb.api.repo.OdbCtx
import lucuma.odb.api.schema.DatasetSchema.DatasetType
import lucuma.odb.api.schema.ExecutionEventSchema.{DatasetEventType, StepEventType}
import lucuma.odb.api.schema.syntax.`enum`._
import org.typelevel.log4cats.Logger
import sangria.schema._

object StepRecordSchema {

  import StepSchema.{StepConfigType, StepIdType}
  import TimeSchema.{NonNegativeDurationType, InstantScalar}
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
          fieldType   = ListType(StepEventType[F]),
          description = "Step events associated with this step".some,
          resolve     = _.value.stepEvents
        ),

        Field(
          name        = "stepQaState",
          fieldType   = OptionType(EnumTypeStepQaState),
          description = "Step QA state based on a combination of dataset QA states".some,
          resolve     = _.value.qaState
        ),

        Field(
          name        = "datasetEvents",
          fieldType   = ListType(DatasetEventType[F]),
          description = "Dataset events associated with this step".some,
          resolve     = _.value.datasetEvents
        ),

        Field(
          name        = "datasets",
          fieldType   = ListType(DatasetType[F]),
          description = "Datasets associated with this step".some,
          resolve     = _.value.datasets
        )

      )
    )

}

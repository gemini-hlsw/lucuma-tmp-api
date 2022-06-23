// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.all._
import eu.timepit.refined.types.all.PosInt
import lucuma.core.enums.DatasetQaState
import lucuma.odb.api.model.{DatasetFilename, DatasetModel, Step, WhereDatasetInput}
import lucuma.odb.api.model.format.ScalarFormat
import lucuma.odb.api.model.query.{SelectResult, WhereEqInput, WhereOptionEqInput, WhereOrderInput}
import lucuma.odb.api.repo.OdbCtx
import lucuma.odb.api.schema.ObservationSchema.InputObjectWhereOrderObservationId
import org.typelevel.log4cats.Logger
import sangria.schema._


object DatasetSchema {

  import context._
  import ObservationSchema.ObservationIdType
  import RefinedSchema.PosIntType
  import QuerySchema._
  import StepSchema.StepIdType
  import syntax.`enum`._
  import syntax.inputtype._
  import syntax.scalar._

  implicit val DatasetFilenameScalar: ScalarType[DatasetFilename] =
    ScalarType.fromScalarFormat(
      name         = "DatasetFilename",
      description  = "Dataset filename in standard format",
      scalarFormat = ScalarFormat(DatasetFilename.fromString, "N20210519S0001.fits")
    )

  implicit val EnumTypeDatasetQaState: EnumType[DatasetQaState] =
    EnumType.fromEnumerated(
      "DatasetQaState",
      "Dataset QA State"
    )

  val ArgumentDatasetQaState: Argument[DatasetQaState] =
    Argument(
      name         = "qaState",
      argumentType = EnumTypeDatasetQaState,
      description  = "Dataset QA State"
    )

  val ArgumentDatasetIndex: Argument[PosInt] =
    Argument(
      name         = "index",
      argumentType = PosIntType,
      description  = "Dataset index"
    )

  val ArgumentOptionalDatasetIndex: Argument[Option[PosInt]] =
    Argument(
      name         = "index",
      argumentType = OptionInputType(PosIntType),
      description  = "Dataset index"
    )

  val DatasetIdType: ObjectType[Any, DatasetModel.Id] =
    ObjectType(
      name     = "DatasetId",
      fieldsFn = () => fields(
        Field(
          name        = "observationId",
          fieldType   = ObservationIdType,
          description = "Observation ID".some,
          resolve     = _.value.observationId
        ),

        Field(
          name        = "stepId",
          fieldType   = StepIdType,
          description = "Step ID".some,
          resolve     = _.value.stepId
        ),

        Field(
          name        = "index",
          fieldType   = PosIntType,
          description = "Dataset index".some,
          resolve     = _.value.index
        )
      )
    )

  implicit val InputObjectDatasetId: InputObjectType[DatasetModel.Id] =
    InputObjectType[DatasetModel.Id](
      "DatasetIdInput",
      "Dataset ID input type",
      List(
        InputField("observationId", ObservationIdType, "Associated observation id."),
        InputField("stepId", StepIdType, "Associated step id."),
        InputField("index", PosIntType, "Dataset index within the step.")
      )
    )

  implicit val InputObjectWhereOrderDatasetId: InputObjectType[WhereOrderInput[DatasetModel.Id]] =
    inputObjectWhereOrder[DatasetModel.Id]("DatasetId", InputObjectDatasetId)

  implicit val InputObjectWhereEqStepId: InputObjectType[WhereEqInput[Step.Id]] =
    inputObjectWhereEq("StepId", StepIdType)

  implicit val InputObjectWhereOrderDatasetIndex: InputObjectType[WhereOrderInput[PosInt]] =
    inputObjectWhereOrder("DatasetIndex", PosIntType)

  implicit val InputObjectWhereOptionEqQaState: InputObjectType[WhereOptionEqInput[DatasetQaState]] =
    inputObjectWhereOptionEq("QaState", EnumTypeDatasetQaState)

  implicit val InputObjectWhereDataset: InputObjectType[WhereDatasetInput] =
    InputObjectType[WhereDatasetInput](
      "WhereDataset",
      "Dataset filter options.  All specified items must match.",
      List(
        InputObjectWhereOrderObservationId.optionField("observationId", "Matches the dataset observation id."),
        InputObjectWhereEqStepId.optionField("stepId", "Matches the dataset step id."),
        InputObjectWhereOrderDatasetIndex.optionField("index", "Matches the dataset index within the step."),
        InputObjectWhereString.optionField("filename", "Matches the dataset file name."),
        InputObjectWhereOptionEqQaState.optionField("qaState", "Matches the dataset QA state.")
      )
    )

  def DatasetType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], DatasetModel] =
    ObjectType(
      name     = "Dataset",
      fieldsFn = () => fields(

        Field(
          name        = "observation",
          fieldType   = ObservationSchema.ObservationType[F],
          description = Some("Observation associated with this dataset"),
          resolve     = c => c.observation(_.unsafeSelect(c.value.id.observationId, includeDeleted = true))
        ),

        Field(
          name        = "id",
          fieldType   = DatasetIdType,
          description = "Dataset id".some,
          resolve     = _.value.id
        ),

        Field(
          name        = "filename",
          fieldType   = DatasetFilenameScalar,
          description = "Dataset filename".some,
          resolve     = _.value.dataset.filename
        ),

        Field(
          name        = "qaState",
          fieldType   = OptionType(EnumTypeDatasetQaState),
          description = "Dataset QA state".some,
          resolve     = _.value.dataset.qaState
        )

      )
    )

  implicit def DatasetSelectResult[F[_]: Dispatcher: Async: Logger]: ObjectType[Any, SelectResult[DatasetModel]] =
    SelectResultType[DatasetModel]("dataset", DatasetType[F])

}

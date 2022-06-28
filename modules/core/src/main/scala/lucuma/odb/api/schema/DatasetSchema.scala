// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.all._
import eu.timepit.refined.types.all.PosInt
import lucuma.core.enums.DatasetQaState
import lucuma.odb.api.model.{DatasetFilename, DatasetModel, WhereDatasetInput}
import lucuma.odb.api.model.format.ScalarFormat
import lucuma.odb.api.model.query.{SizeLimitedResult, WhereOptionEqInput, WhereOrderInput}
import lucuma.odb.api.repo.OdbCtx
import lucuma.odb.api.schema.ObservationSchema.InputObjectWhereOrderObservationId
import org.typelevel.log4cats.Logger
import sangria.marshalling.circe._
import sangria.schema._


object DatasetSchema {

  import context._
  import ObservationSchema.{ObservationIdArgument, ObservationIdType}
  import RefinedSchema.{NonNegIntType, PosIntType}
  import QuerySchema._
  import StepSchema.{ArgumentStepId, StepIdType, InputObjectWhereEqStepId}
  import syntax.`enum`._
  import syntax.inputobjecttype._
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

  implicit val ArgumentOptionWhereDataset: Argument[Option[WhereDatasetInput]] =
    Argument(
      name         = "WHERE",
      argumentType = OptionInputType(InputObjectWhereDataset),
      description  = "Filters the selection of datasets."
    )

  implicit val ArgumentOptionOffsetDataset: Argument[Option[DatasetModel.Id]] =
    Argument(
      name         = "OFFSET",
      argumentType = OptionInputType(InputObjectDatasetId),
      description  = "Starts the result set at (or after if not existent) the given dataset id."
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

  implicit def DatasetSelectResult[F[_]: Dispatcher: Async: Logger]: ObjectType[Any, SizeLimitedResult[DatasetModel]] =
    SelectResultType[DatasetModel]("dataset", DatasetType[F])

  def dataset[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "dataset",
      fieldType   = OptionType(DatasetType[F]),
      description = "Select the dataset associated with the given observation, step, and index".some,
      arguments   = List(
        ObservationIdArgument,
        ArgumentStepId,
        ArgumentDatasetIndex
      ),
      resolve    = c => {
        val id = DatasetModel.Id(c.observationId, c.stepId, c.arg(ArgumentDatasetIndex))
        c.dataset(_.selectDataset(id))
      }
    )

  def datasets[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "datasets",
      fieldType   =  DatasetSelectResult[F],
      description = "Select all datasets associated with a step or observation".some,
      arguments   = List(
        ArgumentOptionWhereDataset,
        ArgumentOptionOffsetDataset,
        ArgumentOptionLimit
      ),
      resolve     = c => {
        val where = c.arg(ArgumentOptionWhereDataset).getOrElse(WhereDatasetInput.MatchAll)
        val off   = c.arg(ArgumentOptionOffsetDataset)
        val limit = c.resultSetLimit
        c.dataset(_.selectWhere(where, off, limit))
      }
    )

  def queryFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      dataset,
      datasets
    )

  implicit val InputObjectTypeDatasetProperties: InputObjectType[DatasetModel.PropertiesInput] =
    InputObjectType[DatasetModel.PropertiesInput](
      "DatasetPropertiesInput",
      "Editable dataset properties",
      List(
        EnumTypeDatasetQaState.optionField("qaState")
      )
    )

  val InputObjectTypeUpdateDatasets: InputObjectType[DatasetModel.UpdateInput] =
    InputObjectType[DatasetModel.UpdateInput](
      "UpdateDatasetsInput",
      "Dataset selection and update description. Use `SET` to specify the changes, `WHERE` to select the datasets to update, and `LIMIT` to control the size of the return value.",
      List(
        InputField("SET",  InputObjectTypeDatasetProperties, "Describes the dataset values to modify."),
        InputObjectWhereDataset.optionField("WHERE", "Filters the datasets to be updated according to those that match the given constraints."),
        NonNegIntType.optionField("LIMIT", "Caps the number of results returned to the given value (if additional datasets match the WHERE clause they will be updated but not returned).")
      )
    )

  val ArgumentUpdateDatasets: Argument[DatasetModel.UpdateInput] =
    InputObjectTypeUpdateDatasets.argument(
      "input",
      "Parameters for editing existing datasets"
    )

  def updateDatasets[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name      = "updateDatasets",
      fieldType = UpdateResultType("UpdateDatasetsResult", "datasets", DatasetType[F]),
      arguments = List(ArgumentUpdateDatasets),
      resolve   = c => c.dataset(_.update(c.arg(ArgumentUpdateDatasets)))
    )

  def mutationFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      updateDatasets
    )

}

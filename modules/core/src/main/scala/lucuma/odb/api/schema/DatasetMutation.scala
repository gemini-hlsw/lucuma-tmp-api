// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import lucuma.odb.api.model.DatasetModel
import lucuma.odb.api.repo.OdbCtx
import lucuma.odb.api.schema.DatasetSchema.InputObjectWhereDataset
import lucuma.odb.api.schema.syntax.inputobjecttype._
import lucuma.odb.api.schema.syntax.inputtype._
import org.typelevel.log4cats.Logger
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._

trait DatasetMutation {

  import DatasetSchema.{EnumTypeDatasetQaState, DatasetType}
  import ObservationSchema.ObservationIdType
  import QuerySchema.UpdateResultType
  import RefinedSchema.{NonNegIntType, PosIntType}
  import StepSchema.StepIdType
  import context._

  implicit val InputObjectTypeDatasetId: InputObjectType[DatasetModel.Id] =
    deriveInputObjectType[DatasetModel.Id](
      InputObjectTypeName("DatasetModelIdInput"),
      InputObjectTypeDescription("Dataset model id creation parameters")
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
      fieldType = UpdateResultType("datasets", DatasetType[F]),
      arguments = List(ArgumentUpdateDatasets),
      resolve   = c => c.dataset(_.update(c.arg(ArgumentUpdateDatasets)))
    )

  def allFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      updateDatasets
    )

}

object DatasetMutation extends DatasetMutation
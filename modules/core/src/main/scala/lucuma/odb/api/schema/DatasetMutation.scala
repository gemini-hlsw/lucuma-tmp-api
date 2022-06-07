// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.option._
import lucuma.odb.api.model.DatasetModel
import lucuma.odb.api.repo.OdbCtx
import lucuma.odb.api.schema.syntax.inputobjecttype._
import lucuma.odb.api.schema.syntax.inputtype._
import org.typelevel.log4cats.Logger
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._

trait DatasetMutation {

  import DatasetSchema.{EnumTypeDatasetQaState, DatasetType}
  import ObservationSchema.ObservationIdType
  import RefinedSchema.PosIntType
  import StepSchema.StepIdType
  import context._

  implicit val InputObjectTypeDatasetId: InputObjectType[DatasetModel.Id] =
    deriveInputObjectType[DatasetModel.Id](
      InputObjectTypeName("DatasetModelIdInput"),
      InputObjectTypeDescription("Dataset model id creation parameters")
    )

  implicit val InputObjectTypeDatasetSelect: InputObjectType[DatasetModel.SelectInput] =
    InputObjectType[DatasetModel.SelectInput](
      "DatasetSelectInput",
      """Choose observationId to include all of its datasets, add a step id to
        |limit it to a particular step, and an index to limit to a particular
        |dataset produced by the step.
        |""".stripMargin,
      List(
        InputField("observationId", ObservationIdType),
        StepIdType.optionField("stepId"),
        PosIntType.optionField("index")
      )
    )

  implicit val InputObjectTypeDatasetProperties: InputObjectType[DatasetModel.PropertiesInput] =
    InputObjectType[DatasetModel.PropertiesInput](
      "DatasetPropertiesInput",
      "Editable dataset properties",
      List(
        EnumTypeDatasetQaState.optionField("qaState")
      )
    )

  val InputObjectTypeDatasetEdit: InputObjectType[DatasetModel.EditInput] =
    InputObjectType[DatasetModel.EditInput](
      "EditDatasetInput",
      "Dataset selection and update description",
      List(
        InputField("select", InputObjectTypeDatasetSelect),
        InputField("patch",  InputObjectTypeDatasetProperties)
      )
    )

  val ArgumentDatasetEdit: Argument[DatasetModel.EditInput] =
    InputObjectTypeDatasetEdit.argument(
      "input",
      "Parameters for editing existing datasets"
    )

  def EditDatasetsResultType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], DatasetModel.EditResult] =
    ObjectType(
      name        = "EditDatasetsResult",
      description = "The result of editing the selected datasets.",
      fieldsFn    = () => fields(

        Field(
          name        = "datasets",
          description = "The edited datasets.".some,
          fieldType   = ListType(DatasetType[F]),
          resolve     = _.value.datasets
        )
      )
    )

  def editDatasets[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name      = "editDatasets",
      fieldType = EditDatasetsResultType[F],
      arguments = List(
        ArgumentDatasetEdit
      ),
      resolve   = c => c.dataset(_.edit(c.arg(ArgumentDatasetEdit)))
    )

  def allFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      editDatasets
    )

}

object DatasetMutation extends DatasetMutation
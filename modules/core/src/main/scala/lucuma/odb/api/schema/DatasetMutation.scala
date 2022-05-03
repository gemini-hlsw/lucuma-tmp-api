// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import lucuma.odb.api.model.DatasetModel
import lucuma.odb.api.model.DatasetModel.SetDatasetQaStateInput
import lucuma.odb.api.repo.OdbCtx
import org.typelevel.log4cats.Logger
import sangria.marshalling.circe._
import sangria.schema._

trait DatasetMutation {

  import DatasetSchema.{DatasetType, EnumTypeDatasetQaState}
  import RefinedSchema.PosIntType
  import StepSchema.StepIdType
  import context._
  import syntax.inputobjecttype._

  val InputObjectDatasetId: InputObjectType[DatasetModel.Id] =
    InputObjectType[DatasetModel.Id](
      "DatasetIdInput",
      "Identifies a dataset in an argument to a mutation",
      List(
        InputField("stepId", StepIdType),
        InputField("index",  PosIntType)
      )
    )

  val InputObjectSetDatasetQaState: InputObjectType[SetDatasetQaStateInput] =
    InputObjectType[SetDatasetQaStateInput](
      "SetDatasetQaStateInput",
      "Defines the QA State and the datasets to which it applies",
      List(
        InputField("qaState",    EnumTypeDatasetQaState),
        InputField("datasetIds", ListInputType(InputObjectDatasetId))
      )
    )


  val SetDatasetQaStateArgument: Argument[SetDatasetQaStateInput] =
    InputObjectSetDatasetQaState.argument(
      "SetDatasetQaState",
      "Arguments for setting the QA state of one or more datasets"
    )

  def setDatasetQaState[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name      = "setDatasetQaState",
      fieldType = ListType(DatasetType[F]),
      arguments = List(SetDatasetQaStateArgument),
      resolve   = c => {
        val s = c.arg(SetDatasetQaStateArgument)
        c.dataset(_.markQaState(s.qaState, s.datasetIds))
      }
    )

  def allFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      setDatasetQaState
    )

}

object DatasetMutation extends DatasetMutation
// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{DatasetModel, WhereDatasetInput}
import lucuma.odb.api.repo.OdbCtx
import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.option._
import lucuma.odb.api.schema.QuerySchema.ArgumentOptionLimit
import org.typelevel.log4cats.Logger
import sangria.marshalling.circe._
import sangria.schema._

trait DatasetQuery {

  import context._
  import DatasetSchema._
  import ObservationSchema.ObservationIdArgument
  import StepSchema.ArgumentStepId

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

  def allFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      dataset,
      datasets
    )

}

object DatasetQuery extends DatasetQuery

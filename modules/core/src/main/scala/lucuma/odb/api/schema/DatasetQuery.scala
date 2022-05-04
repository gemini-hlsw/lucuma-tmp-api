// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.DatasetModel
import lucuma.odb.api.repo.OdbCtx
import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.option._
import org.typelevel.log4cats.Logger
import sangria.schema._

trait DatasetQuery {

  import context._
  import DatasetSchema._
  import ObservationSchema.ObservationIdArgument
  import Paging._
  import StepSchema.{ArgumentStepId, ArgumentOptionalStepId}

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
      fieldType   =  DatasetConnectionType[F],
      description = "Select all datasets associated with a step or observation".some,
      arguments   = List(
        ObservationIdArgument,
        ArgumentOptionalStepId,
        ArgumentPagingFirst,
        ArgumentPagingCursor
      ),
      resolve     = c =>
        unsafeSelectPageFuture[F, DatasetModel.Id, DatasetModel](
          c.pagingCursor("id")(DatasetIdCursor.getOption),
          dm => DatasetIdCursor.reverseGet(dm.id),
          o  => c.ctx.odbRepo.dataset.selectDatasetsPage(c.observationId, c.optionalStepId, c.pagingFirst, o)
        )
    )

  def allFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      dataset,
      datasets
    )

}

object DatasetQuery extends DatasetQuery

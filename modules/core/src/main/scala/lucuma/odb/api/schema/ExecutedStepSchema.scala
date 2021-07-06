// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{DatasetModel, ExecutedStepModel}
import lucuma.odb.api.repo.OdbRepo

import cats.MonadError
import cats.effect.std.Dispatcher
import cats.syntax.all._
import eu.timepit.refined.types.all.PosInt
import sangria.schema._

object ExecutedStepSchema {

  import context._
  import Paging._

  import DatasetSchema.{ DatasetConnectionType, IndexCursor }
  import AtomSchema.AtomInterfaceType
  import StepSchema.{StepIdType, StepInterfaceType}

  def ExecutedStepType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], ExecutedStepModel] = {
    ObjectType(
      name        = "ExecutedStep",
      description = s"Executed step",
      fieldsFn    = () => fields(

        Field(
          name        = "id",
          fieldType   = StepIdType,
          description = "Step id".some,
          resolve     = _.value.stepId
        ),

        Field(
          name        = "step",
          fieldType   = StepInterfaceType[F],
          description = "The executed step itself".some,
          resolve     = c => c.step(_.unsafeSelectStep(c.value.stepId))
        ),

        Field(
          name        = "atom",
          fieldType   = AtomInterfaceType[F],
          description = "The atom containing the executed step".some,
          resolve     = c => c.atom(_.unsafeSelectAtom(c.value.atomId))
        ),

        Field(
          name        = "datasets",
          fieldType   = DatasetConnectionType[F],
          description = "Datasets associated with this step".some,
          arguments   = List(
            ArgumentPagingFirst,
            ArgumentPagingCursor
          ),
          resolve     = c =>
            unsafeSelectPageFuture[F, PosInt, DatasetModel](
              c.pagingCursor("index")(IndexCursor.getOption),
              dm => IndexCursor.reverseGet(dm.index),
              o  => c.ctx.executionEvent.selectDatasetsPageForStep(c.value.stepId, c.pagingFirst, o)
            )
        )

      )
    )
  }

  def ExecutedStepEdgeType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], Paging.Edge[ExecutedStepModel]] =
    Paging.EdgeType(
      "ExecutedStepEdge",
      "An executed step and its cursor",
      ExecutedStepType[F]
    )

  def ExecutedStepConnectionType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], Paging.Connection[ExecutedStepModel]] =
    Paging.ConnectionType(
      "ExecutedStepConnection",
      "Executed steps in the current page",
      ExecutedStepType[F],
      ExecutedStepEdgeType[F]
    )

}

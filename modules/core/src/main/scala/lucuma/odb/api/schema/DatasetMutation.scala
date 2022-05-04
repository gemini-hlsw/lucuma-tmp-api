// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import lucuma.odb.api.repo.OdbCtx
import org.typelevel.log4cats.Logger
import sangria.schema._

trait DatasetMutation {

  import DatasetSchema.{ArgumentDatasetQaState, ArgumentOptionalDatasetIndex, DatasetType}
  import ObservationSchema.ObservationIdArgument
  import StepSchema.ArgumentOptionalStepId
  import context._

  def setDatasetQaState[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name      = "setDatasetQaState",
      fieldType = ListType(DatasetType[F]),
      arguments = List(
        ObservationIdArgument,
        ArgumentOptionalStepId,
        ArgumentOptionalDatasetIndex,
        ArgumentDatasetQaState
      ),
      resolve   = c => c.dataset(
        _.markQaState(
          c.observationId,
          c.optionalStepId,
          c.arg(ArgumentOptionalDatasetIndex),
          c.arg(ArgumentDatasetQaState)
        )
      )
    )

  def allFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      setDatasetQaState
    )

}

object DatasetMutation extends DatasetMutation
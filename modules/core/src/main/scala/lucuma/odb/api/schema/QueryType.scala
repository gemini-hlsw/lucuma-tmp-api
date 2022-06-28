// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import lucuma.odb.api.repo.OdbCtx
import org.typelevel.log4cats.Logger
import sangria.schema._

/**
 * Queries offered by the API.
 */
object QueryType {

  def apply[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], Unit] =
    ObjectType(
      name   = "Query",
      fields = (
        ObservationSchema.queryFields[F]   ++
        ProgramQuery.allFields[F]          ++
        TargetQuery.allFields[F]           ++
        DatasetSchema.queryFields[F]       ++
        ExecutionEventSchema.queryFields[F]
      ).sortBy(_.name)
    )

}

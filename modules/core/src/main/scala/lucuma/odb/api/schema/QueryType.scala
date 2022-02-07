// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.repo.OdbRepo
import cats.effect.Async
import cats.effect.std.Dispatcher
import org.typelevel.log4cats.Logger
import sangria.schema._

/**
 * Queries offered by the API.
 */
object QueryType {

  def apply[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbRepo[F], Unit] =
    ObjectType(
      name   = "Query",
      fields =
        ObservationQuery.allFields[F]   ++
        ProgramQuery.allFields[F]       ++
        TargetQuery.allFields[F]
    )

}

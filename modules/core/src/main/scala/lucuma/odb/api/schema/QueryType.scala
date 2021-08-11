// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.repo.OdbRepo

import cats.MonadError
import cats.Parallel
import cats.effect.std.Dispatcher
import sangria.schema._
import lucuma.odb.itc.Itc

/**
 * Queries offered by the API.
 */
object QueryType {

  def apply[F[_]: Parallel: Dispatcher: Itc](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], Unit] =
    ObjectType(
      name   = "Query",
      fields =
        AsterismQuery.allFields[F]      ++
        ConfigurationQuery.allFields[F] ++
        ObservationQuery.allFields[F]   ++
        ProgramQuery.allFields[F]       ++
        TargetQuery.allFields[F]
    )

}

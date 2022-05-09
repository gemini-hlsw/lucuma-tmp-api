// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import lucuma.odb.api.repo.OdbCtx
import org.typelevel.log4cats.Logger
import sangria.schema._

/**
 * Mutations offered by the API.
 */
object MutationType {

  def apply[F[_]: Dispatcher: Async: Logger](
    testing: Boolean
  ): ObjectType[OdbCtx[F], Unit] =
    ObjectType(
      name   = "Mutation",
      fields = (
        ProgramMutation.allFields[F]                 ++
        ObservationMutation.allFields[F]             ++
        TargetMutation.allFields[F]                  ++
        ExecutionEventMutation.allFields[F](testing) ++
        DatasetMutation.allFields[F]
      ).sortBy(_.name)
    )

}

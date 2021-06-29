// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.repo.OdbRepo

import cats.MonadError
import cats.effect.std.Dispatcher
import org.typelevel.log4cats.Logger
import sangria.schema._

/**
 * Mutations offered by the API.
 */
object MutationType {

  def apply[F[_]: Logger: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], Unit] =
    ObjectType(
      name   = "Mutation",
      fields =
        AsterismMutation.allFields[F]      ++
        ObservationMutation.allFields[F]   ++
        TargetMutation.allFields[F]        ++
        SharingMutation.allFields[F]       ++
        ExecutionEventMutation.allFields[F]

    )

}

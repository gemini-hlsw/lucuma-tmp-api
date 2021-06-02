// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.repo.OdbRepo
import cats.effect.Effect
import sangria.schema._

/**
 * Queries offered by the API.
 */
object QueryType {

  def apply[F[_]: Effect]: ObjectType[OdbRepo[F], Unit] =
    ObjectType(
      name   = "Query",
      fields =
        AsterismQuery.allFields[F]      ++
        ConfigurationQuery.allFields[F] ++
        ConstraintSetQuery.allFields[F] ++
        ObservationQuery.allFields[F]   ++
        ProgramQuery.allFields[F]       ++
        TargetQuery.allFields[F]
    )

}

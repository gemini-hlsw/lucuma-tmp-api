// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.repo.OdbRepo
import cats.effect.Effect
import sangria.schema._

trait ObservationQuery {

  import GeneralSchema.ArgumentIncludeDeleted
  import ProgramSchema.ProgramIdArgument
  import ObservationSchema.{ObservationIdArgument, ObservationType}
  import context._

  def allForProgram[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name        = "observations",
      fieldType   = ListType(ObservationType[F]),
      description = Some("Returns all observations associated with the given program."),
      arguments   = List(ProgramIdArgument, ArgumentIncludeDeleted),
      resolve     = c => c.observation(_.selectAllForProgram(c.programId, c.includeDeleted))
    )

  def forId[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name        = "observation",
      fieldType   = OptionType(ObservationType[F]),
      description = Some("Returns the observation with the given id, if any."),
      arguments   = List(ObservationIdArgument, ArgumentIncludeDeleted),
      resolve     = c => c.observation(_.select(c.observationId, c.includeDeleted))
    )

  def allFields[F[_]: Effect]: List[Field[OdbRepo[F], Unit]] =
    List(
      allForProgram,
      forId
    )
}

object ObservationQuery extends ObservationQuery

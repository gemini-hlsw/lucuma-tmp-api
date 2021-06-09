// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.repo.OdbRepo
import cats.effect.Effect
import cats.syntax.all._
import sangria.schema._

trait ObservationQuery {

  import ConstraintSetSchema.ConstraintSetIdArgument
  import GeneralSchema.ArgumentIncludeDeleted
  import Paging._
  import ProgramSchema.OptionalProgramIdArgument
  import ObservationSchema.{ObservationIdArgument, ObservationType, ObservationConnectionType, OptionalListObservationIdArgument}
  import context._

  def observations[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name        = "observations",
      fieldType   = ObservationConnectionType[F],
      description = "Returns all observations associated with the given ids or program, or all observations if neither is specified.".some,
      arguments   = List(
        OptionalListObservationIdArgument.copy(description = "(Optional) listing of specific observations to retrieve".some),
        OptionalProgramIdArgument.copy(description = "(Optional) program whose observations are sought".some),
        ArgumentPagingFirst,
        ArgumentPagingCursor,
        ArgumentIncludeDeleted
      ),
      resolve     = c =>
        unsafeSelectTopLevelPageFuture(c.pagingObservationId) { gid =>
          (c.arg(OptionalListObservationIdArgument), c.arg(OptionalProgramIdArgument)) match {
            case (Some(oids), _) =>
              c.ctx.observation.selectPageForObservations(oids.toSet, c.pagingFirst, gid, c.includeDeleted)
            case (_, Some(pid))  =>
              c.ctx.observation.selectPageForProgram(pid, c.pagingFirst, gid, c.includeDeleted)
            case _               =>
              c.ctx.observation.selectPage(c.pagingFirst, gid, c.includeDeleted)
          }
        }
    )

  def allForConstraintSet[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name        = "observations",
      fieldType   = ObservationConnectionType[F],
      description = "Returns all observations associated with the give constraint set.".some,
      arguments   = List(
        ConstraintSetIdArgument,
        ArgumentPagingFirst,
        ArgumentPagingCursor,
        ArgumentIncludeDeleted
      ),
      resolve     = c => unsafeSelectTopLevelPageFuture(c.pagingObservationId) { gid =>
        c.ctx.observation.selectPageForConstraintSet(c.constraintSetId, c.pagingFirst, gid, c.includeDeleted)
      }
    )

  def forId[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name        = "observation",
      fieldType   = OptionType(ObservationType[F]),
      description = "Returns the observation with the given id, if any.".some,
      arguments   = List(ObservationIdArgument, ArgumentIncludeDeleted),
      resolve     = c => c.observation(_.select(c.observationId, c.includeDeleted))
    )

  def allFields[F[_]: Effect]: List[Field[OdbRepo[F], Unit]] =
    List(
      observations,
      forId
    )
}

object ObservationQuery extends ObservationQuery

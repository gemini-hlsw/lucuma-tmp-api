// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.model.Observation
import lucuma.odb.api.model.{ConstraintSetModel, InputError, ObservationModel}
import lucuma.odb.api.repo.{OdbRepo, ResultPage}

import cats.effect.Effect
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all._
import sangria.schema._

trait ObservationQuery {

  import ConstraintSetSchema.ConstraintSetGroupConnectionType
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
            case (Some(_), Some(_)) =>
              Effect[F].raiseError[ResultPage[ObservationModel]](
                InputError.fromMessage(
                  s"Specify only one of `${OptionalListObservationIdArgument.name}` or `${OptionalProgramIdArgument.name}`"
                ).toException
              )
            case (Some(oids), None) =>
              c.ctx.observation.selectPageForObservations(oids.toSet, c.pagingFirst, gid, c.includeDeleted)
            case (None, Some(pid))  =>
              c.ctx.observation.selectPageForProgram(pid, c.pagingFirst, gid, c.includeDeleted)
            case (None, None)       =>
              c.ctx.observation.selectPage(c.pagingFirst, gid, c.includeDeleted)
          }
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

  def groupByConstraintSet[F[_]: Effect]: Field[OdbRepo[F], Unit] = {
    Field(
      name        = "constraintGroups",
      fieldType   = ConstraintSetGroupConnectionType[F],
      description = Some("Observations group by commonly held constraints"),
      arguments   = List(
        ArgumentPagingFirst,
        ArgumentPagingCursor,
        ArgumentIncludeDeleted
      ),
      resolve    = c => {
        val all: F[List[ConstraintSetModel.Group]] =
          c.ctx.tables.get.map { t =>
            t.observations
             .groupBy { case (_, o) => o.constraintSet }
             .view
             .mapValues(_.keySet)
             .toList
             .sortBy(_._2.head)
             .map { case (c, oids) => ConstraintSetModel.Group(c, oids) }
          }

        Paging.unsafeSelectPageFuture[F, Observation.Id, ConstraintSetModel.Group](
          c.pagingObservationId,
          g => Cursor.gid[Observation.Id].reverseGet(g.observationIds.head),
          oid => all.map { gs =>
            ResultPage.fromSeq(gs, c.arg(ArgumentPagingFirst), oid, _.observationIds.head)
          }

        )

      }

    )
  }

  def allFields[F[_]: Effect]: List[Field[OdbRepo[F], Unit]] =
    List(
      observations,
      forId,
      groupByConstraintSet
    )
}

object ObservationQuery extends ObservationQuery

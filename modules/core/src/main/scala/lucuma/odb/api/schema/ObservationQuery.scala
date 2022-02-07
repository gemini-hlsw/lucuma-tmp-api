// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{ConstraintSetModel, InputError, ObservationModel, ScienceRequirements}
import lucuma.odb.api.repo.{OdbRepo, ResultPage}
import cats.MonadError
import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.all._
import lucuma.core.model.{Observation, Target}
import lucuma.odb.api.model.targetModel.{TargetEnvironmentModel, TargetModel}
import lucuma.odb.api.schema.TargetSchema.TargetEnvironmentType
import org.typelevel.log4cats.Logger
import sangria.schema._

trait ObservationQuery {

  import ConstraintSetSchema.ConstraintSetType
  import context._
  import GeneralSchema.ArgumentIncludeDeleted
  import Paging._
  import ProgramSchema.OptionalProgramIdArgument
  import ObservationSchema.{ObservationIdArgument, ObservationType, ObservationConnectionType, OptionalListObservationIdArgument}
  import ScienceRequirementsSchema.ScienceRequirementsType
  import TargetSchema.TargetType

  def observations[F[_]: Dispatcher: Async: Logger]: Field[OdbRepo[F], Unit] =
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
              MonadError[F, Throwable].raiseError[ResultPage[ObservationModel]](
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

  def forId[F[_]: Dispatcher: Async: Logger]: Field[OdbRepo[F], Unit] =
    Field(
      name        = "observation",
      fieldType   = OptionType(ObservationType[F]),
      description = "Returns the observation with the given id, if any.".some,
      arguments   = List(ObservationIdArgument, ArgumentIncludeDeleted),
      resolve     = c => c.observation(_.select(c.observationId, c.includeDeleted))
    )

  def groupByTarget[F[_]: Dispatcher: Async: Logger]: Field[OdbRepo[F], Unit] =

    ObservationGroupSchema.groupingField[F, TargetModel, Target.Id](
      "target",
      "Observations grouped by commonly held targets",
      TargetType[F],
      (repo, pid, includeDeleted) => repo.groupByTargetInstantiated(pid, includeDeleted),
      _.pagingTargetId,
      _.value.id
    )

  def groupByAsterism[F[_]: Dispatcher: Async: Logger]: Field[OdbRepo[F], Unit] =

    ObservationGroupSchema.groupingField[F, Seq[TargetModel], Observation.Id](
      "asterism",
      "Observations grouped by commonly held science asterisms",
      ListType(TargetType[F]),
      (repo, pid, includeDeleted) => repo.groupByAsterismInstantiated(pid, includeDeleted),
      _.pagingObservationId,
      _.observationIds.head
    )

  def groupByTargetEnvironment[F[_]: Dispatcher: Async: Logger]: Field[OdbRepo[F], Unit] =

    ObservationGroupSchema.groupingField[F, TargetEnvironmentModel, Observation.Id](
      "targetEnvironment",
      "Observations grouped by common target environment",
      TargetEnvironmentType[F],
      (repo, pid, includeDeleted) => repo.groupByTargetEnvironment(pid, includeDeleted),
      _.pagingObservationId,
      _.observationIds.head
    )

  def groupByConstraintSet[F[_]: Dispatcher: Async: Logger]: Field[OdbRepo[F], Unit] =

    ObservationGroupSchema.groupingField[F, ConstraintSetModel, Observation.Id](
      "constraintSet",
      "Observations grouped by commonly held constraints",
      ConstraintSetType[F],
      (repo, pid, includeDeleted) => repo.groupByConstraintSet(pid, includeDeleted),
      _.pagingObservationId,
      _.observationIds.head
    )

  def groupByScienceRequirements[F[_]: Dispatcher: Async: Logger]: Field[OdbRepo[F], Unit] =

    ObservationGroupSchema.groupingField[F, ScienceRequirements, Observation.Id](
      "scienceRequirements",
      "Observations grouped by commonly held science requirements",
      ScienceRequirementsType[F],
      (repo, pid, includeDeleted) => repo.groupByScienceRequirements(pid, includeDeleted),
      _.pagingObservationId,
      _.observationIds.head
    )

  def allFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbRepo[F], Unit]] =
    List(
      observations,
      forId,
      groupByTarget,
      groupByAsterism,
      groupByTargetEnvironment,
      groupByConstraintSet,
      groupByScienceRequirements
    )
}

object ObservationQuery extends ObservationQuery

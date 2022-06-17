// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{ObservationModel, ScienceMode, ScienceRequirements, WhereObservationInput}
import lucuma.odb.api.repo.OdbCtx
import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.all._
import lucuma.core.model.{ConstraintSet, Observation, Target}
import lucuma.odb.api.model.query.SelectResult
import lucuma.odb.api.model.targetModel.{TargetEnvironmentModel, TargetModel}
import lucuma.odb.api.schema.QuerySchema.DefaultLimit
import lucuma.odb.api.schema.TargetSchema.TargetEnvironmentType
import org.typelevel.log4cats.Logger
import sangria.marshalling.circe._
import sangria.schema._

trait ObservationQuery {

  import ConstraintSetSchema.ConstraintSetType
  import context._
  import GeneralSchema.ArgumentIncludeDeleted
  import ObservationSchema.{InputObjectWhereObservation, ObservationIdArgument, ObservationIdType, ObservationType}
  import QuerySchema.{ArgumentOptionLimit, SelectResultType}
  import ScienceModeSchema.ScienceModeType
  import ScienceRequirementsSchema.ScienceRequirementsType
  import TargetSchema.TargetType

  implicit val ArgumentOptionWhereObservation: Argument[Option[WhereObservationInput]] =
    Argument(
      name         = "WHERE",
      argumentType = OptionInputType(InputObjectWhereObservation),
      description  = "Filters the selection of observations."
    )

  implicit val ArgumentOptionOffsetObservation: Argument[Option[Observation.Id]] =
    Argument(
      name         = "OFFSET",
      argumentType = OptionInputType(ObservationIdType),
      description  = "Starts the result set at (or after if not existent) the given observation id."
    )

  implicit def ObservationSelectResult[F[_]: Dispatcher: Async: Logger]: ObjectType[Any, SelectResult[ObservationModel]] =
    SelectResultType[ObservationModel]("observation", ObservationType[F])

  def observations[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "observations",
      fieldType   = ObservationSelectResult[F],
      description = "Selects the first `LIMIT` matching observations based on the provided `WHERE` parameter, if any.".some,
      arguments   = List(
        ArgumentOptionWhereObservation,
        ArgumentOptionOffsetObservation,
        ArgumentOptionLimit
      ),
      resolve     = c =>
        c.observation(_.selectWhere(c.arg(ArgumentOptionWhereObservation), c.arg(ArgumentOptionOffsetObservation), c.arg(ArgumentOptionLimit).getOrElse(DefaultLimit)))
    )

  def forId[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "observation",
      fieldType   = OptionType(ObservationType[F]),
      description = "Returns the observation with the given id, if any.".some,
      arguments   = List(ObservationIdArgument, ArgumentIncludeDeleted),
      resolve     = c => c.observation(_.select(c.observationId, c.includeDeleted))
    )

  def groupByTarget[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =

    ObservationGroupSchema.groupingField[F, TargetModel, Target.Id](
      "target",
      "Observations grouped by commonly held targets",
      TargetType[F],
      (repo, pid, includeDeleted) => repo.groupByTargetInstantiated(pid, includeDeleted),
      _.pagingTargetId,
      _.value.id
    )

  def groupByAsterism[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =

    ObservationGroupSchema.groupingField[F, Seq[TargetModel], Observation.Id](
      "asterism",
      "Observations grouped by commonly held science asterisms",
      ListType(TargetType[F]),
      (repo, pid, includeDeleted) => repo.groupByAsterismInstantiated(pid, includeDeleted),
      _.pagingObservationId,
      _.observationIds.head
    )

  def groupByTargetEnvironment[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =

    ObservationGroupSchema.groupingField[F, TargetEnvironmentModel, Observation.Id](
      "targetEnvironment",
      "Observations grouped by common target environment",
      TargetEnvironmentType[F],
      (repo, pid, includeDeleted) => repo.groupByTargetEnvironment(pid, includeDeleted),
      _.pagingObservationId,
      _.observationIds.head
    )

  def groupByConstraintSet[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =

    ObservationGroupSchema.groupingField[F, ConstraintSet, Observation.Id](
      "constraintSet",
      "Observations grouped by commonly held constraints",
      ConstraintSetType,
      (repo, pid, includeDeleted) => repo.groupByConstraintSet(pid, includeDeleted),
      _.pagingObservationId,
      _.observationIds.head
    )

  def groupByScienceMode[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =

    ObservationGroupSchema.groupingField[F, Option[ScienceMode], Observation.Id](
      "scienceMode",
      "Observations grouped by commonly held science mode",
      OptionType(ScienceModeType),
      (repo, pid, includeDeleted) => repo.groupByScienceMode(pid, includeDeleted),
      _.pagingObservationId,
      _.observationIds.head
    )

  def groupByScienceRequirements[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =

    ObservationGroupSchema.groupingField[F, ScienceRequirements, Observation.Id](
      "scienceRequirements",
      "Observations grouped by commonly held science requirements",
      ScienceRequirementsType[F],
      (repo, pid, includeDeleted) => repo.groupByScienceRequirements(pid, includeDeleted),
      _.pagingObservationId,
      _.observationIds.head
    )

  def allFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      observations,
      forId,
      groupByTarget,
      groupByAsterism,
      groupByTargetEnvironment,
      groupByConstraintSet,
      groupByScienceMode,
      groupByScienceRequirements
    )
}

object ObservationQuery extends ObservationQuery

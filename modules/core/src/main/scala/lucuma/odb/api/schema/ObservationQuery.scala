// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{ObservationModel, ScienceMode, ScienceRequirements, WhereObservationInput}
import lucuma.odb.api.repo.OdbCtx
import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.all._
import lucuma.core.model.{ConstraintSet, Observation}
import lucuma.odb.api.model.query.SizeLimitedResult
import lucuma.odb.api.model.targetModel.{TargetEnvironmentModel, TargetModel}
import lucuma.odb.api.schema.TargetSchema.TargetEnvironmentType
import org.typelevel.log4cats.Logger
import sangria.marshalling.circe._
import sangria.schema._

trait ObservationQuery {

  import ConstraintSetSchema.ConstraintSetType
  import context._
  import GeneralSchema.ArgumentIncludeDeleted
  import ObservationSchema.{InputObjectWhereObservation, ArgumentObservationId, ObservationIdType, ObservationType}
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

  implicit def ObservationSelectResult[F[_]: Dispatcher: Async: Logger]: ObjectType[Any, SizeLimitedResult[ObservationModel]] =
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
      resolve     = c => {
        val where = c.arg(ArgumentOptionWhereObservation).getOrElse(WhereObservationInput.MatchPresent)
        val off   = c.arg(ArgumentOptionOffsetObservation)
        val limit = c.resultSetLimit
        c.observation(_.selectWhere(where, off, limit))
      }
    )

  def forId[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "observation",
      fieldType   = OptionType(ObservationType[F]),
      description = "Returns the observation with the given id, if any.".some,
      arguments   = List(ArgumentObservationId, ArgumentIncludeDeleted),
      resolve     = c => c.observation(_.select(c.observationId, c.includeDeleted))
    )

  def groupByTarget[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =

    ObservationGroupSchema.groupingField[F, TargetModel](
      "target",
      "Observations grouped by commonly held targets",
      TargetType[F],
      (repo, pid, where) => repo.groupByTargetInstantiated(pid, where)
    )

  def groupByAsterism[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =

    ObservationGroupSchema.groupingField[F, Seq[TargetModel]](
      "asterism",
      "Observations grouped by commonly held science asterisms",
      ListType(TargetType[F]),
      (repo, pid, where) => repo.groupByAsterismInstantiated(pid, where)
    )

  def groupByTargetEnvironment[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =

    ObservationGroupSchema.groupingField[F, TargetEnvironmentModel](
      "targetEnvironment",
      "Observations grouped by common target environment",
      TargetEnvironmentType[F],
      (repo, pid, where) => repo.groupByTargetEnvironment(pid, where)
    )

  def groupByConstraintSet[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =

    ObservationGroupSchema.groupingField[F, ConstraintSet](
      "constraintSet",
      "Observations grouped by commonly held constraints",
      ConstraintSetType,
      (repo, pid, where) => repo.groupByConstraintSet(pid, where)
    )

  def groupByScienceMode[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =

    ObservationGroupSchema.groupingField[F, Option[ScienceMode]](
      "scienceMode",
      "Observations grouped by commonly held science mode",
      OptionType(ScienceModeType),
      (repo, pid, where) => repo.groupByScienceMode(pid, where)
    )

  def groupByScienceRequirements[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =

    ObservationGroupSchema.groupingField[F, ScienceRequirements](
      "scienceRequirements",
      "Observations grouped by commonly held science requirements",
      ScienceRequirementsType[F],
      (repo, pid, where) => repo.groupByScienceRequirements(pid, where)
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

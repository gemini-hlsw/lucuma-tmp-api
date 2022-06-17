// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.all._
import lucuma.core.model.Target
import lucuma.odb.api.model.query.SelectResult
import lucuma.odb.api.repo.OdbCtx
import lucuma.odb.api.model.targetModel.{TargetModel, WhereTargetInput}
import lucuma.odb.api.schema.QuerySchema.DefaultLimit
import org.typelevel.log4cats.Logger
import sangria.marshalling.circe._
import sangria.schema._

trait TargetQuery {
  import context._

  import GeneralSchema.ArgumentIncludeDeleted
  import ObservationSchema.ObservationIdArgument
  import TargetSchema.{ArgumentTargetId, InputObjectWhereTarget, TargetEnvironmentType, TargetIdType, TargetType}
  import QuerySchema.{ArgumentOptionLimit, SelectResultType}

  implicit val ArgumentOptionWhereTarget: Argument[Option[WhereTargetInput]] =
    Argument(
      name         = "WHERE",
      argumentType = OptionInputType(InputObjectWhereTarget),
      description  = "Filters the selection of targets."
    )

  implicit val ArgumentOptionOffsetTarget: Argument[Option[Target.Id]] =
    Argument(
      name         = "OFFSET",
      argumentType = OptionInputType(TargetIdType),
      description  = "Starts the result set at (or after if not existent) the given target id."
    )

  def target[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "target",
      fieldType   = OptionType(TargetType[F]),
      description = "Retrieves the target with the given id, if it exists".some,
      arguments   = List(
        ArgumentTargetId,
        ArgumentIncludeDeleted
      ),
      resolve     = c => c.target(_.select(c.targetId, c.includeDeleted))
    )

  implicit def TargetSelectResult[F[_]: Dispatcher: Async: Logger]: ObjectType[Any, SelectResult[TargetModel]] =
    SelectResultType[TargetModel]("target", TargetType[F])

  def targets[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "targets",
      fieldType   = TargetSelectResult[F],
      description = "Selects the first `LIMIT` matching targets based on the provided `WHERE` parameter, if any.".some,
      arguments   = List(ArgumentOptionWhereTarget, ArgumentOptionOffsetTarget, ArgumentOptionLimit),
      resolve     = c => c.target(_.selectWhere(c.arg(ArgumentOptionWhereTarget), c.arg(ArgumentOptionOffsetTarget), c.arg(ArgumentOptionLimit).getOrElse(DefaultLimit)))
    )

  def asterism[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "asterism",
      fieldType   = ListType(TargetType[F]),
      description = "All science targets (if any) for the given observation (or environment)".some,
      arguments   = List(ObservationIdArgument, ArgumentIncludeDeleted),
      resolve     = c => c.target(_.selectObservationAsterism(c.observationId, c.includeDeleted).map(_.toList))
    )

  def targetEnvironment[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "targetEnvironment",
      fieldType   = OptionType(TargetEnvironmentType[F]),
      description = "Target environment for the given observation (or environment id)".some,
      arguments   = List(ObservationIdArgument),
      resolve     = c => c.target(_.selectObservationTargetEnvironment(c.observationId))
    )

  def allFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      target[F],
      targets[F],
      asterism[F],
      targetEnvironment[F]
    )
}

object TargetQuery extends TargetQuery

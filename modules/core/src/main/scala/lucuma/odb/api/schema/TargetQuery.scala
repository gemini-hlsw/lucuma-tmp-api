// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.all._
import lucuma.odb.api.repo.{OdbRepo, ResultPage}
import lucuma.odb.api.model.targetModel.TargetModel
import lucuma.odb.api.schema.TargetSchema.ArgumentTargetId
import org.typelevel.log4cats.Logger
import sangria.schema._

trait TargetQuery {
  import context._

  import GeneralSchema.ArgumentIncludeDeleted
  import ObservationSchema.{ ObservationIdArgument, OptionalListObservationIdArgument }
  import Paging._
  import ProgramSchema.OptionalProgramIdArgument
  import TargetSchema.{TargetEnvironmentType, TargetConnectionType, TargetType}

  def target[F[_]: Dispatcher: Async: Logger]: Field[OdbRepo[F], Unit] =
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

  def scienceTargets[F[_]: Dispatcher: Async: Logger]: Field[OdbRepo[F], Unit] =
    Field(
      name        = "scienceTargets",
      fieldType   = TargetConnectionType[F],
      description = "All the science targets associated with a given program or specific observations".some,
      arguments   = List(
        OptionalProgramIdArgument,
        OptionalListObservationIdArgument,
        ArgumentPagingFirst,
        ArgumentPagingCursor,
        ArgumentIncludeDeleted
      ),
      resolve = c =>
        unsafeSelectTopLevelPageFuture(c.pagingTargetId) { gid =>
          (c.optionalProgramId, c.arg(OptionalListObservationIdArgument)) match {
            case (_, Some(oids)) => c.ctx.target.selectPageForObservations(oids.toSet, c.pagingFirst, gid, c.includeDeleted)
            case (Some(pid), _)  => c.ctx.target.selectPageForProgram(pid, c.pagingFirst, gid, c.includeDeleted)
            case _               => ResultPage.empty[TargetModel].pure[F]
          }
        }
    )

  def firstScienceTarget[F[_]: Dispatcher: Async: Logger]: Field[OdbRepo[F], Unit] =
    Field(
      name        = "firstScienceTarget",
      fieldType   = OptionType(TargetType[F]),
      description = "The first (or only) science target (if any) for the given observation.  This will essentially pick a random target from the observation's asterism and is meant as a convenience when there is only one target.".some,
      arguments   = List(ObservationIdArgument, ArgumentIncludeDeleted),
      resolve     = c => c.target(_.selectObservationFirstTarget(c.observationId))
    )

  def asterism[F[_]: Dispatcher: Async: Logger]: Field[OdbRepo[F], Unit] =
    Field(
      name        = "asterism",
      fieldType   = ListType(TargetType[F]),
      description = "All science targets (if any) for the given observation (or environment)".some,
      arguments   = List(ObservationIdArgument, ArgumentIncludeDeleted),
      resolve     = c => c.target(_.selectObservationAsterism(c.observationId, c.includeDeleted).map(_.toList))
    )

  def targetEnvironment[F[_]: Dispatcher: Async: Logger]: Field[OdbRepo[F], Unit] =
    Field(
      name        = "targetEnvironment",
      fieldType   = OptionType(TargetEnvironmentType[F]),
      description = "Target environment for the given observation (or environment id)".some,
      arguments   = List(ObservationIdArgument),
      resolve     = c => c.target(_.selectObservationTargetEnvironment(c.observationId))
    )

  def allFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbRepo[F], Unit]] =
    List(
      target[F],
      scienceTargets[F],
      firstScienceTarget[F],
      asterism[F],
      targetEnvironment[F]
    )
}

object TargetQuery extends TargetQuery

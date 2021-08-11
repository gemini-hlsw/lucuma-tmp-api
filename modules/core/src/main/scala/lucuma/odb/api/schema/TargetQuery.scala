// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.repo.OdbRepo
import cats.MonadError
import cats.effect.std.Dispatcher
import cats.syntax.all._
import sangria.schema._


trait TargetQuery {
  import context._

  import ObservationSchema.ObservationIdArgument

  import TargetSchema.{TargetEnvironmentType, TargetType}

  def scienceTarget[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name        = "scienceTarget",
      fieldType   = OptionType(TargetType[F]),
      description = "The first (or only) science target (if any) for the given observation".some,
      arguments   = List(ObservationIdArgument),
      resolve     = c => c.observation { repo =>
        repo
          .select(c.observationId, includeDeleted = true)
          .map(_.flatMap(_.targets.science.values.headOption))
      }
    )

  def scienceTargets[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name        = "scienceTargets",
      fieldType   = ListType(TargetType[F]),
      description = "All science targets (if any) for the given observation".some,
      arguments   = List(ObservationIdArgument),
      resolve     = c => c.observation { repo =>
        repo
          .select(c.observationId, includeDeleted = true)
          .map(_.toList.flatMap(_.targets.science.values))
      }
    )

  def targetEnvironment[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name        = "targetEnvironment",
      fieldType   = OptionType(TargetEnvironmentType[F]),
      description = "Target environment for the given observation".some,
      arguments   = List(ObservationIdArgument),
      resolve     = c => c.observation { repo =>
        repo
          .select(c.observationId, includeDeleted = true)
          .map(_.map(_.targets))
      }
    )

  def allFields[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): List[Field[OdbRepo[F], Unit]] =
    List(
      scienceTarget[F],
      scienceTargets[F],
      targetEnvironment[F]
    )
}

object TargetQuery extends TargetQuery

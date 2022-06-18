// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.all._
import lucuma.core.model.Program
import lucuma.odb.api.model.{ObservationModel, WhereObservationInput}
import lucuma.odb.api.model.query.SelectResult
import lucuma.odb.api.repo.{ObservationRepo, OdbCtx}
import lucuma.odb.api.schema.QuerySchema.{ArgumentOptionLimit, DefaultLimit}
import org.typelevel.log4cats.Logger
import sangria.schema._


object ObservationGroupSchema {

  import context._
  import GeneralSchema.ArgumentIncludeDeleted
  import ObservationSchema.{ObservationConnectionType, ObservationIdType}
  import ObservationQuery.ArgumentOptionWhereObservation
  import Paging._
  import ProgramSchema.ProgramIdArgument
  import QuerySchema.SelectResultType

  def ObservationGroupType[F[_]: Dispatcher: Async: Logger, A](
    prefix:      String,
    valueName:   String,
    outType:     OutputType[A]
  ): ObjectType[OdbCtx[F], ObservationModel.Group[A]] =

    ObjectType(
      name     = s"${prefix}Group",
      fieldsFn = () => fields(

        Field(
          name        = "observationIds",
          fieldType   = ListType(ObservationIdType),
          description = "IDs of observations that use the same constraints".some,
          resolve     = _.value.observationIds.toList
        ),

        Field(
          name        = "observations",
          fieldType   = ObservationConnectionType[F],
          description = "Observations associated with the common value".some,
          arguments   = List(
            ArgumentPagingFirst,
            ArgumentPagingCursor,
            ArgumentIncludeDeleted
          ),
          resolve     = c =>
            unsafeSelectTopLevelPageFuture(c.pagingObservationId) { gid =>
              c.ctx.odbRepo.observation.selectPageFromIds(c.pagingFirst, gid, c.includeDeleted) { _ =>
                c.value.observationIds
              }
            }
        ),

        Field(
          name        = valueName,
          fieldType   = outType,
          description = "Commonly held value across the observations".some,
          resolve     = _.value.value
        )

      )
    )

  def ObservationGroupEdgeType[F[_], A](
    groupType: ObjectType[OdbCtx[F], ObservationModel.Group[A]]
  ): ObjectType[OdbCtx[F], Paging.Edge[ObservationModel.Group[A]]] =

    Paging.EdgeType[F, ObservationModel.Group[A]](
      s"${groupType.name}Edge",
      "An observation group and its cursor",
      groupType
    )

  def ObservationGroupConnectionType[F[_], A](
    groupType: ObjectType[OdbCtx[F], ObservationModel.Group[A]],
    edgeType:  ObjectType[OdbCtx[F], Paging.Edge[ObservationModel.Group[A]]]
  ): ObjectType[OdbCtx[F], Paging.Connection[ObservationModel.Group[A]]] =

    Paging.ConnectionType[F, ObservationModel.Group[A]](
      s"${groupType.name}Connection",
      "Observations grouped by common properties",
      groupType,
      edgeType
    )

  def groupingField[F[_]: Dispatcher: Async: Logger, A](
    name:        String,
    description: String,
    outType:     OutputType[A],
    lookupAll:   (ObservationRepo[F], Program.Id, Option[WhereObservationInput]) => F[List[ObservationModel.Group[A]]]
  ): Field[OdbCtx[F], Unit] = {

    val groupType =
      ObservationGroupSchema.ObservationGroupType[F, A](
        name.capitalize,
        name,
        outType
      )

    val selectResultType =
      SelectResultType[ObservationModel.Group[A]](name, groupType)

    Field(
      name        = s"${name}Group",
      fieldType   = selectResultType,
      description = description.some,
      arguments   = List(
        ProgramIdArgument,
        ArgumentOptionWhereObservation,
        ArgumentOptionLimit
      ),
      resolve    = c => c.unsafeToFuture {

        val limit = c.arg(ArgumentOptionLimit).getOrElse(DefaultLimit).value

        lookupAll(c.ctx.odbRepo.observation, c.programId, c.arg(ArgumentOptionWhereObservation)).map { gs =>
          val (result, rest) = gs.splitAt(limit)

          SelectResult.Standard(
            result,
            rest.nonEmpty
          )
        }
      }
    )
  }

}

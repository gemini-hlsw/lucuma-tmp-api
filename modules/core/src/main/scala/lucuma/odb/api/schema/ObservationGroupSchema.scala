// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{InputError, ObservationModel}
import lucuma.odb.api.repo.{ObservationRepo, ResultPage}
import cats.Order.catsKernelOrderingForOrder
import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.all._
import lucuma.core.model.Program
import lucuma.core.util.Gid
import org.typelevel.log4cats.Logger
import sangria.schema._


object ObservationGroupSchema {

  import context._
  import GeneralSchema.ArgumentIncludeDeleted
  import ObservationSchema.{ObservationConnectionType, ObservationIdType}
  import Paging._
  import ProgramSchema.ProgramIdArgument

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
          description = Some("IDs of observations that use the same constraints"),
          resolve     = _.value.observationIds.toList
        ),

        Field(
          name        = "observations",
          fieldType   = ObservationConnectionType[F],
          description = Some("Observations that use this constraint set"),
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

  def groupingField[F[_]: Dispatcher: Async: Logger, A, G: Gid](
    name:        String,
    description: String,
    outType:     OutputType[A],
    lookupAll:   (ObservationRepo[F], Program.Id, Boolean) => F[List[ObservationModel.Group[A]]],
    cursor:      Context[OdbCtx[F], Unit] => Either[InputError, Option[G]],
    gid:         ObservationModel.Group[A] => G
  ): Field[OdbCtx[F], Unit] = {

    val groupType =
      ObservationGroupSchema.ObservationGroupType[F, A](
        name.capitalize,
        name,
        outType
      )

    val edgeType =
      ObservationGroupSchema.ObservationGroupEdgeType[F, A](
        groupType
      )

    val connectionType =
      ObservationGroupSchema.ObservationGroupConnectionType[F, A](
        groupType,
        edgeType
      )

    Field(
      name        = s"${name}Group",
      fieldType   = connectionType,
      description = description.some,
      arguments   = List(
        ProgramIdArgument,
        ArgumentPagingFirst,
        ArgumentPagingCursor,
        ArgumentIncludeDeleted
      ),
      resolve    = c =>
        Paging.unsafeSelectPageFuture[F, G, ObservationModel.Group[A]](
          cursor(c),
          grp   => Cursor.gid[G].reverseGet(gid(grp)),
          after => lookupAll(c.ctx.odbRepo.observation, c.programId, c.includeDeleted).map { gs =>
            ResultPage.fromSeq(gs.sortBy(gid), c.arg(ArgumentPagingFirst), after, gid)
          }
        )
    )
  }

}

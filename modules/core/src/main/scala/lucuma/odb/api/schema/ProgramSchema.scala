// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.ProgramModel
import lucuma.odb.api.repo.OdbRepo
import lucuma.core.model.Program

import cats.MonadError
import cats.effect.std.Dispatcher
import cats.syntax.foldable._
import cats.syntax.functor._
import sangria.schema._

import scala.collection.immutable.Seq

object ProgramSchema {

  import GeneralSchema.{ArgumentIncludeDeleted, EnumTypeExistence, NonEmptyStringType, PlannedTimeSummaryType}
  import ObservationSchema.ObservationConnectionType
  import Paging._
  import context._

  implicit val ProgramIdType: ScalarType[Program.Id] =
    ObjectIdSchema.idType[Program.Id]("ProgramId")

  val ProgramIdArgument: Argument[Program.Id] =
    Argument(
      name         = "programId",
      argumentType = ProgramIdType,
      description  = "Program ID"
    )

  val OptionalProgramIdArgument: Argument[Option[Program.Id]] =
    Argument(
      name         = "programId",
      argumentType = OptionInputType(ProgramIdType),
      description  = "Program ID"
    )

  val OptionalListProgramIdArgument: Argument[Option[Seq[Program.Id]]] =
    Argument(
      name         = "programIds",
      argumentType = OptionInputType(ListInputType(ProgramIdType)),
      description  = "Program Ids"
    )

  def ProgramType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], ProgramModel] =
    ObjectType(
      name     = "Program",
      fieldsFn = () => fields(

        Field(
          name        = "id",
          fieldType   = ProgramIdType,
          description = Some("Program ID"),
          resolve     = _.value.id
        ),

        Field(
          name        = "existence",
          fieldType   = EnumTypeExistence,
          description = Some("Deleted or Present"),
          resolve     = _.value.existence
        ),

        Field(
          name        = "name",
          fieldType   = OptionType(NonEmptyStringType),
          description = Some("Program name"),
          resolve     = _.value.name
        ),

        Field(
          name        = "observations",
          fieldType   = ObservationConnectionType[F],
          description = Some("All observations associated with the program (needs pagination)."),
          arguments   = List(
            ArgumentPagingFirst,
            ArgumentPagingCursor,
            ArgumentIncludeDeleted
          ),
          resolve     = c =>
            unsafeSelectTopLevelPageFuture(c.pagingObservationId) { gid =>
              c.ctx.observation.selectPageForProgram(c.value.id, c.pagingFirst, gid, c.includeDeleted)
            }
        ),

        Field(
          name        = "plannedTime",
          fieldType   = PlannedTimeSummaryType[F],
          description = Some("Program planned time calculation."),
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c => c.observation {
            _.selectPageForProgram(c.value.id, Some(Integer.MAX_VALUE), None, c.includeDeleted)
             .map(_.nodes.foldMap(_.plannedTimeSummary))
          }
        )


      )
    )

  def ProgramEdgeType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], Edge[ProgramModel]] =
    EdgeType(
      "ProgramEdge",
      "A Program node and its cursor",
      ProgramType[F]
    )

  def ProgramConnectionType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], Connection[ProgramModel]] =
    ConnectionType(
      "ProgramConnection",
      "Programs in the current page",
      ProgramType[F],
      ProgramEdgeType[F]
    )

}

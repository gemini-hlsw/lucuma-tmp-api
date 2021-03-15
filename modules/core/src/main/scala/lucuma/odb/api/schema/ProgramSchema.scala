// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.ProgramModel
import lucuma.odb.api.repo.OdbRepo
import lucuma.core.model.Program
import cats.effect.Effect
import cats.syntax.foldable._
import cats.syntax.functor._
import sangria.schema._

object ProgramSchema {

  import AsterismSchema.AsterismConnectionType
  import GeneralSchema.{ArgumentIncludeDeleted, EnumTypeExistence, NonEmptyStringType, PlannedTimeSummaryType}
  import ObservationSchema.ObservationConnectionType
  import Paging._
  import TargetSchema.TargetConnectionType
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

  def ProgramType[F[_]: Effect]: ObjectType[OdbRepo[F], ProgramModel] =
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

        // Targets are extracted from the "database" as they are not directly
        // referenced in the Program model class itself.

        // QUESTION: I didn't bother with DeferredResolvers because it is an
        // in-memory "repository" anyway.  Is there any reason we should use
        // them for this toy service?

        Field(
          name        = "asterisms",
          fieldType   = AsterismConnectionType[F],
          description = Some("All asterisms associated with the program (needs pagination)."),
          arguments   = List(
            ArgumentPagingFirst,
            ArgumentPagingCursor,
            ArgumentIncludeDeleted
          ),
          resolve     = c =>
            unsafeSelectPageFuture(c.pagingAsterismId) { gid =>
              c.ctx.asterism.selectPageForProgram(c.value.id, c.pagingFirst, gid, c.includeDeleted)
            }
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
            unsafeSelectPageFuture(c.pagingObservationId) { gid =>
              c.ctx.observation.selectPageForProgram(c.value.id, c.pagingFirst, gid, c.includeDeleted)
            }
        ),

        Field(
          name        = "targets",
          fieldType   = TargetConnectionType[F],
          description = Some("All targets associated with the program (needs pagination)."),
          arguments   = List(
            ArgumentPagingFirst,
            ArgumentPagingCursor,
            ArgumentIncludeDeleted
          ),
          resolve     = c =>
            unsafeSelectPageFuture(c.pagingTargetId) { gid =>
              c.ctx.target.selectPageForProgram(c.value.id, c.pagingFirst, gid, c.includeDeleted)
            }
        ),

        Field(
          name        = "plannedTime",
          fieldType   = PlannedTimeSummaryType[F],
          description = Some("Program planned time calculation."),
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c => c.observation {
            _.selectPageForProgram(c.value.id, Integer.MAX_VALUE, None, c.includeDeleted)
             .map(_.nodes.foldMap(_.plannedTimeSummary))
          }
        )


      )
    )

  def ProgramEdgeType[F[_]: Effect]: ObjectType[OdbRepo[F], Edge[ProgramModel]] =
    EdgeType(
      "ProgramEdge",
      "A Program node and its cursor",
      ProgramType[F]
    )

  def ProgramConnectionType[F[_]: Effect]: ObjectType[OdbRepo[F], Connection[ProgramModel]] =
    ConnectionType(
      "ProgramConnection",
      "Programs in the current page",
      ProgramType[F],
      ProgramEdgeType[F]
    )

}

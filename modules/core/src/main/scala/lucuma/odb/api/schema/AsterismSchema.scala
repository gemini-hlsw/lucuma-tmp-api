// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.AsterismModel
import lucuma.odb.api.repo.OdbRepo
import lucuma.core.model.Asterism
import cats.effect.Effect
import sangria.schema._

object AsterismSchema {

  import GeneralSchema.{EnumTypeExistence, ArgumentIncludeDeleted}
  import ObservationSchema.ObservationConnectionType
  import Paging._
  import ProgramSchema.{OptionalProgramIdArgument, ProgramConnectionType}
  import TargetSchema.{CoordinateType, TargetConnectionType}
  import context._

  implicit val AsterismIdType: ScalarType[Asterism.Id] =
    ObjectIdSchema.idType[Asterism.Id](name = "AsterismId")

  val AsterismIdArgument: Argument[Asterism.Id] =
    Argument(
      name         = "asterismId",
      argumentType = AsterismIdType,
      description  = "Asterism ID"
    )

  val OptionalAsterismIdArgument: Argument[Option[Asterism.Id]] =
    Argument(
      name         = "asterismId",
      argumentType = OptionInputType(AsterismIdType),
      description  = "Asterism ID"
    )

  def AsterismType[F[_]: Effect]: ObjectType[OdbRepo[F], AsterismModel] =
    ObjectType[OdbRepo[F], AsterismModel](
      name        = "Asterism",
      description = "Collection of stars observed in a single observation",

      fields[OdbRepo[F], AsterismModel](
        Field(
          name        = "id",
          fieldType   = AsterismIdType,
          description = Some("Asterism ID"),
          resolve     = _.value.id
        ),

        Field(
          name        = "existence",
          fieldType   = EnumTypeExistence,
          description = Some("Whether the asterism is deleted or present"),
          resolve     = _.value.existence
        ),

        Field(
          name        = "name",
          fieldType   = OptionType(StringType),
          description = Some("Asterism name, if any."),
          resolve     = _.value.name.map(_.value)
        ),

        Field(
          name        = "explicitBase",
          fieldType   = OptionType(CoordinateType[F]),
          description = Some("When set, overrides the default base position of the asterism"),
          resolve     = _.value.explicitBase
        ),

        Field(
          name        = "observations",
          fieldType   = ObservationConnectionType[F],
          arguments   = List(
            OptionalProgramIdArgument,
            ArgumentPagingFirst,
            ArgumentPagingCursor,
            ArgumentIncludeDeleted
          ),
          description = Some("All observations associated with the asterism."),
          resolve     = c =>
            unsafeSelectPageFuture(c.pagingObservationId) { gid =>
              c.ctx.observation.selectPageForAsterism(c.value.id, c.optionalProgramId, c.pagingFirst, gid, c.includeDeleted)
            }
        ),

        Field(
          name        = "targets",
          fieldType   = TargetConnectionType[F],
          arguments   = List(
            ArgumentPagingFirst,
            ArgumentPagingCursor,
            ArgumentIncludeDeleted
          ),
          description = Some("All asterism targets"),
          resolve     = c =>
            unsafeSelectPageFuture(c.pagingTargetId) { gid =>
              c.ctx.target.selectPageForAsterism(c.value.id, c.pagingFirst, gid, c.includeDeleted)
            }
        ),

        Field(
          name        = "programs",
          fieldType   = ProgramConnectionType[F],
          arguments   = List(
            ArgumentPagingFirst,
            ArgumentPagingCursor,
            ArgumentIncludeDeleted
          ),
          description = Some("The programs associated with the asterism."),
          resolve     = c =>
            unsafeSelectPageFuture(c.pagingProgramId) { gid =>
              c.ctx.program.selectPageForAsterism(c.value.id, includeObservations = true, c.pagingFirst, gid, c.includeDeleted)
            }
        )
      )
    )

  def AsterismEdgeType[F[_]: Effect]: ObjectType[OdbRepo[F], Paging.Edge[AsterismModel]] =
    Paging.EdgeType(
      "AsterismEdge",
      "An Asterism and its cursor",
      AsterismType[F]
    )

  def AsterismConnectionType[F[_]: Effect]: ObjectType[OdbRepo[F], Paging.Connection[AsterismModel]] =
    Paging.ConnectionType(
      "AsterismConnection",
      "Asterisms in the current page",
      AsterismType[F],
      AsterismEdgeType[F]
    )

}

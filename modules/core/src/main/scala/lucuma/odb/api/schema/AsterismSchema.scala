// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.AsterismModel
import lucuma.odb.api.repo.OdbRepo

import cats.effect.Effect
import cats.implicits._
import cats.effect.implicits._
import sangria.schema._

object AsterismSchema {

  import TargetSchema.{CoordinateType, TargetType}
  import GeneralSchema.{EnumTypeExistence, ArgumentIncludeDeleted}

  implicit val AsterismIdType: ScalarType[AsterismModel.Id] =
    ObjectIdSchema.idType[AsterismModel.Id](name = "AsterismId")

  val AsterismIdArgument: Argument[AsterismModel.Id] =
    Argument(
      name         = "id",
      argumentType = AsterismIdType,
      description  = "Asterism ID"
    )

  def AsterismType[F[_]: Effect]: InterfaceType[OdbRepo[F], AsterismModel] =
    InterfaceType[OdbRepo[F], AsterismModel](
      name        = "Asterism",
      description = "Common fields shared by all asterisms",
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
          name        = "explicitBase",
          fieldType   = OptionType(CoordinateType[F]),
          description = Some("When set, overrides the default base position of the asterism"),
          resolve     = _.value.explicitBase
        ),

        Field(
          name        = "targets",
          fieldType   = ListType(TargetType[F]),
          arguments   = List(ArgumentIncludeDeleted),
          description = Some("All asterism targets"),
          resolve     = c =>
            c.value
             .targets
             .traverse(c.ctx.target.select(_, c.arg(ArgumentIncludeDeleted)))
             .map(_.flatMap(_.toList))
             .toIO
             .unsafeToFuture()
        )
      )
    )


}

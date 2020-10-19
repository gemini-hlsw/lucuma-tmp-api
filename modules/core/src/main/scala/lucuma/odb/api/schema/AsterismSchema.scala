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

  import GeneralSchema.{EnumTypeExistence, ArgumentIncludeDeleted}
  import ObservationSchema.ObservationType
  import ProgramSchema.{OptionalProgramIdArgument, ProgramType}
  import TargetSchema.{CoordinateType, TargetType}
  import context._

  implicit val AsterismIdType: ScalarType[AsterismModel.Id] =
    ObjectIdSchema.idType[AsterismModel.Id](name = "AsterismId")

  val AsterismIdArgument: Argument[AsterismModel.Id] =
    Argument(
      name         = "aid",
      argumentType = AsterismIdType,
      description  = "Asterism ID"
    )

  val OptionalAsterismIdArgument: Argument[Option[AsterismModel.Id]] =
    Argument(
      name         = "aid",
      argumentType = OptionInputType(AsterismIdType),
      description  = "Asterism ID"
    )

  def AsterismType[F[_]: Effect]: InterfaceType[OdbRepo[F], AsterismModel] =
    InterfaceType[OdbRepo[F], AsterismModel](
      name        = "Asterism",
      description = "Common fields shared by all asterisms",
      fields[OdbRepo[F], AsterismModel](
        Field(
          name        = "aid",
          fieldType   = AsterismIdType,
          description = Some("Asterism ID"),
          resolve     = _.value.aid
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
          name        = "observations",
          fieldType   = ListType(ObservationType[F]),
          arguments   = List(OptionalProgramIdArgument, ArgumentIncludeDeleted),
          description = Some("All observations associated with the asterism."),
          resolve     = c => c.observation(
            _.selectAllForAsterism(c.value.aid, c.includeDeleted)
             .map { obsList =>
               c.optionalProgramId.fold(obsList) { pid => obsList.filter(_.pid === pid) }
             }
          )
        ),

        Field(
          name        = "targets",
          fieldType   = ListType(TargetType[F]),
          arguments   = List(ArgumentIncludeDeleted),
          description = Some("All asterism targets"),
          resolve     = c =>
            c.value
             .targets
             .iterator
             .toList
             .traverse(c.ctx.target.select(_, c.includeDeleted))
             .map(_.flatMap(_.toList))
             .toIO
             .unsafeToFuture()
        ),

        Field(
          name        = "programs",
          fieldType   = ListType(ProgramType[F]),
          arguments   = List(ArgumentIncludeDeleted),
          description = Some("The programs associated with the asterism."),
          resolve     = c => c.program(_.selectAllForAsterism(c.value.aid, c.includeDeleted))
        )
      )
    )

  def DefaultAsterismType[F[_]: Effect]: ObjectType[OdbRepo[F], AsterismModel.Default] =
    ObjectType[OdbRepo[F], AsterismModel.Default](
      name        = "DefaultAsterism",
      description = "Default asterism",
      interfaces  = List(PossibleInterface.apply[OdbRepo[F], AsterismModel.Default](AsterismType[F])),
      fields      = Nil
    )


}

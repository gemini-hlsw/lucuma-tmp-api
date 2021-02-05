// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.AsterismModel
import lucuma.odb.api.repo.OdbRepo
import lucuma.core.model.Asterism
import cats.effect.Effect
import cats.implicits._
import sangria.schema._

object AsterismSchema {

  import GeneralSchema.{EnumTypeExistence, ArgumentIncludeDeleted}
  import ObservationSchema.ObservationType
  import ProgramSchema.{OptionalProgramIdArgument, ProgramType}
  import TargetSchema.{CoordinateType, TargetType}
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
          fieldType   = ListType(ObservationType[F]),
          arguments   = List(OptionalProgramIdArgument, ArgumentIncludeDeleted),
          description = Some("All observations associated with the asterism."),
          resolve     = c => c.observation(
            _.selectAllForAsterism(c.value.id, c.includeDeleted)
             .map { obsList =>
               c.optionalProgramId.fold(obsList) { pid => obsList.filter(_.programId === pid) }
             }
          )
        ),

        Field(
          name        = "targets",
          fieldType   = ListType(TargetType[F]),
          arguments   = List(ArgumentIncludeDeleted),
          description = Some("All asterism targets"),
          resolve     = c => c.target(
            _.selectAllForAsterism(c.value.id, c.includeDeleted)
          )
        ),

        Field(
          name        = "programs",
          fieldType   = ListType(ProgramType[F]),
          arguments   = List(ArgumentIncludeDeleted),
          description = Some("The programs associated with the asterism."),
          resolve     = c => c.program(_.selectAllForAsterism(c.value.id, c.includeDeleted))
        )
      )
    )

}

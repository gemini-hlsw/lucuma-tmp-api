// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.ProgramModel
import lucuma.odb.api.repo.OdbRepo

import cats.effect.Effect
import sangria.schema._

object ProgramSchema {

  import AsterismSchema.AsterismType
  import GeneralSchema.{EnumTypeExistence, ArgumentIncludeDeleted}
  import ObservationSchema.ObservationType
  import TargetSchema.TargetType
  import context._

  implicit val ProgramIdType: ScalarType[ProgramModel.Id] =
    ObjectIdSchema.idType[ProgramModel.Id]("ProgramId")

  val ProgramIdArgument: Argument[ProgramModel.Id] =
    Argument(
      name         = "id",
      argumentType = ProgramIdType,
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
          fieldType   = OptionType(StringType),
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
          fieldType   = ListType(AsterismType[F]),
          description = Some("All asterisms associated with the program (needs pagination)."),
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c => c.asterism(_.selectAllForProgram(c.value.id, c.includeDeleted))
        ),

        Field(
          name        = "observations",
          fieldType   = ListType(ObservationType[F]),
          description = Some("All observations associated with the program (needs pagination)."),
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c => c.observation(_.selectAllForProgram(c.value.id, c.includeDeleted))
        ),

        Field(
          name        = "targets",
          fieldType   = ListType(TargetType[F]),
          description = Some("All targets associated with the program (needs pagination)."),
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c => c.target(_.selectAllForProgram(c.value.id, c.includeDeleted))
        )

      )
    )
}

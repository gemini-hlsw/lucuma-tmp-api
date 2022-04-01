// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.ProgramModel
import lucuma.odb.api.schema.syntax.inputtype._
import cats.effect.Async
import cats.effect.std.Dispatcher
import lucuma.odb.api.repo.OdbCtx
import lucuma.odb.api.schema.ProgramSchema.ProgramType
import org.typelevel.log4cats.Logger
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._

trait ProgramMutation {

  import GeneralSchema.{EnumTypeExistence, NonEmptyStringType}
  import ProgramSchema.ProgramIdType
  import context._
  import syntax.inputobjecttype._

  val InputObjectTypeProgramCreate: InputObjectType[ProgramModel.Create] =
    deriveInputObjectType[ProgramModel.Create](
      InputObjectTypeName("CreateProgramInput"),
      InputObjectTypeDescription("Program creation parameters")
    )

  val ArgumentProgramCreate: Argument[ProgramModel.Create] =
    InputObjectTypeProgramCreate.argument(
      "input",
      "Program description"
    )

  val InputObjectTypeProgramEdit: InputObjectType[ProgramModel.Edit] =
    deriveInputObjectType[ProgramModel.Edit](
      InputObjectTypeName("EditProgramInput"),
      InputObjectTypeDescription("Edit program"),
      ReplaceInputField("existence",     EnumTypeExistence.notNullableField("existence")),
      ReplaceInputField("name",          NonEmptyStringType.nullableField("name")),
    )

  val ArgumentProgramEdit: Argument[ProgramModel.Edit] =
    InputObjectTypeProgramEdit.argument(
      "input",
      "Edit program"
    )

  def create[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name      = "createProgram",
      fieldType = OptionType(ProgramType[F]),
      arguments = List(ArgumentProgramCreate),
      resolve   = c => c.program(_.insert(c.arg(ArgumentProgramCreate)))
    )

  def update[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name      = "updateProgram",
      fieldType = ProgramType[F],
      arguments = List(ArgumentProgramEdit),
      resolve   = c => c.program(_.edit(c.arg(ArgumentProgramEdit)))
    )

  def allFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      create,
      update
    )

}

object ProgramMutation extends ProgramMutation
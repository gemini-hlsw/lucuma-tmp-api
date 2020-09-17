// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.AsterismModel
import lucuma.odb.api.repo.OdbRepo
import cats.effect.Effect
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._


trait AsterismMutation extends TargetScalars {

  import AsterismSchema.{AsterismIdType, AsterismIdArgument, AsterismType}
  import GeneralSchema.EnumTypeExistence
  import ProgramSchema.ProgramIdType
  import TargetSchema.TargetIdType
  import TargetMutation.InputObjectTypeCoordinates

  import context._
  import syntax.inputobjecttype._

  val InputObjectTypeAsterismCreateDefault: InputObjectType[AsterismModel.CreateDefault] =
    deriveInputObjectType[AsterismModel.CreateDefault](
      InputObjectTypeName("CreateDefaultAsterismInput"),
      InputObjectTypeDescription("Default asterism parameters")
    )

  val ArgumentAsterismCreateDefault: Argument[AsterismModel.CreateDefault] =
    InputObjectTypeAsterismCreateDefault.argument(
      "input",
      "Default Asterism description"
    )

  val InputObjectTypeAsterismEditDefault: InputObjectType[AsterismModel.EditDefault] =
    deriveInputObjectType[AsterismModel.EditDefault](
      InputObjectTypeName("EditDefaultAsterismInput"),
      InputObjectTypeDescription("Default asterism edit")
    )

  val ArgumentAsterismEditDefault: Argument[AsterismModel.EditDefault] =
    InputObjectTypeAsterismEditDefault.argument(
      "input",
      "Edit default asterism"
    )

  def createDefault[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "createDefaultAsterism",
      fieldType = OptionType(AsterismType[F]),
      arguments = List(ArgumentAsterismCreateDefault),
      resolve   = c => c.asterism(_.insert(c.arg(ArgumentAsterismCreateDefault)))
    )

  def updateDefault[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "updateDefaultAsterism",
      fieldType = OptionType(AsterismType[F]),
      arguments = List(ArgumentAsterismEditDefault),
      resolve   = c => c.asterism(_.edit(c.arg(ArgumentAsterismEditDefault)))
    )

  def delete[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "deleteAsterism",
      fieldType = OptionType(AsterismType[F]),
      arguments = List(AsterismIdArgument),
      resolve   = c => c.asterism(_.delete(c.asterismId))
    )

  def undelete[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "undeleteAsterism",
      fieldType = OptionType(AsterismType[F]),
      arguments = List(AsterismIdArgument),
      resolve   = c => c.asterism(_.undelete(c.asterismId))
    )

  def allFields[F[_]: Effect]: List[Field[OdbRepo[F], Unit]] =
    List(
      createDefault,
      updateDefault,
      delete,
      undelete
    )
}

object AsterismMutation extends AsterismMutation

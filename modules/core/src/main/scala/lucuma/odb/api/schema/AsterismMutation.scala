// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.AsterismModel
import lucuma.odb.api.repo.OdbRepo

import cats.MonadError
import cats.effect.std.Dispatcher
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._


trait AsterismMutation extends TargetScalars {

  import AsterismSchema.{AsterismIdType, AsterismIdArgument, AsterismType}
  import GeneralSchema.{EnumTypeExistence, NonEmptyStringType}
  import ProgramSchema.ProgramIdType
  import TargetMutation.InputObjectTypeCoordinates

  import context._
  import syntax.inputtype._
  import syntax.inputobjecttype._

  val InputObjectTypeAsterismCreate: InputObjectType[AsterismModel.Create] =
    deriveInputObjectType[AsterismModel.Create](
      InputObjectTypeName("CreateAsterismInput"),
      InputObjectTypeDescription("Asterism parameters")
    )

  val ArgumentAsterismCreate: Argument[AsterismModel.Create] =
    InputObjectTypeAsterismCreate.argument(
      "input",
      "Asterism description"
    )

  val InputObjectTypeAsterismEdit: InputObjectType[AsterismModel.Edit] =
    deriveInputObjectType[AsterismModel.Edit](
      InputObjectTypeName("EditAsterismInput"),
      InputObjectTypeDescription("Asterism edit"),
        ReplaceInputField("existence",    EnumTypeExistence.notNullableField("existence")),
        ReplaceInputField("name",         NonEmptyStringType.nullableField("name")),
        ReplaceInputField("explicitBase", InputObjectTypeCoordinates.nullableField("explicitBase"))
    )

  val ArgumentAsterismEdit: Argument[AsterismModel.Edit] =
    InputObjectTypeAsterismEdit.argument(
      "input",
      "Edit default asterism"
    )

  def create[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "createAsterism",
      fieldType = OptionType(AsterismType[F]),
      arguments = List(ArgumentAsterismCreate),
      resolve   = c => c.asterism[AsterismModel](_.insert(c.arg(ArgumentAsterismCreate)))
    )

  def update[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "updateAsterism",
      fieldType = AsterismType[F],
      arguments = List(ArgumentAsterismEdit),
      resolve   = c => c.asterism[AsterismModel] { r =>
        val ed  = c.arg(ArgumentAsterismEdit)
        r.edit(ed.id, ed.editor)
      }
    )

  def delete[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "deleteAsterism",
      fieldType = AsterismType[F],
      arguments = List(AsterismIdArgument),
      resolve   = c => c.asterism(_.delete(c.asterismId))
    )

  def undelete[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "undeleteAsterism",
      fieldType = AsterismType[F],
      arguments = List(AsterismIdArgument),
      resolve   = c => c.asterism(_.undelete(c.asterismId))
    )

  def allFields[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): List[Field[OdbRepo[F], Unit]] =
    List(
      create,
      update,
      delete,
      undelete
    )
}

object AsterismMutation extends AsterismMutation

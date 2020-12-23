// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{AsterismModel, InputError}
import lucuma.odb.api.repo.{OdbRepo, Tables}
import lucuma.core.model.Target
import cats.effect.Effect
import cats.syntax.option._
import cats.syntax.traverse._
import lucuma.core.util.Gid
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._


trait AsterismMutation extends TargetScalars {

  import AsterismSchema.{AsterismIdType, AsterismIdArgument, AsterismType, DefaultAsterismType}
  import GeneralSchema.EnumTypeExistence
  import ProgramSchema.ProgramIdType
  import TargetSchema.TargetIdType
  import TargetMutation.InputObjectTypeCoordinates

  import context._
  import syntax.inputtype._
  import syntax.inputobjecttype._

  val InputObjectTypeAsterismCreateDefault: InputObjectType[AsterismModel.CreateDefault] =
    deriveInputObjectType[AsterismModel.CreateDefault](
      InputObjectTypeName("CreateDefaultAsterismInput"),
      InputObjectTypeDescription("Default asterism parameters"),
      ReplaceInputField("targetIds",
        InputField(
          name        = "targetIds",
          fieldType   = ListInputType(TargetIdType),
          description = "Targets to include in default asterism"
        )
      )
    )

  val ArgumentAsterismCreateDefault: Argument[AsterismModel.CreateDefault] =
    InputObjectTypeAsterismCreateDefault.argument(
      "input",
      "Default Asterism description"
    )

  val InputObjectTypeAsterismEditDefault: InputObjectType[AsterismModel.EditDefault] =
    deriveInputObjectType[AsterismModel.EditDefault](
      InputObjectTypeName("EditDefaultAsterismInput"),
      InputObjectTypeDescription("Default asterism edit"),
        ReplaceInputField("existence",    EnumTypeExistence.notNullableField("existence")),
        ReplaceInputField("name",         StringType.nullableField("name")),
        ReplaceInputField("explicitBase", InputObjectTypeCoordinates.nullableField("explicitBase")),
        ReplaceInputField("targetIds",
          InputField(
            name        = "targetIds",
            fieldType   = OptionInputType(ListInputType(TargetIdType)),
            description = "Targets to include in the default asterism"
          )
        )
    )

  val ArgumentAsterismEditDefault: Argument[AsterismModel.EditDefault] =
    InputObjectTypeAsterismEditDefault.argument(
      "input",
      "Edit default asterism"
    )

  def createDefault[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "createDefaultAsterism",
      fieldType = OptionType(DefaultAsterismType[F]),
      arguments = List(ArgumentAsterismCreateDefault),
      resolve   = c => c.asterism[AsterismModel.Default](_.insert(c.arg(ArgumentAsterismCreateDefault)))
    )

  def updateDefault[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "updateDefaultAsterism",
      fieldType = DefaultAsterismType[F],
      arguments = List(ArgumentAsterismEditDefault),
      resolve   = c => c.asterism[AsterismModel.Default] { r =>
        val ed     = c.arg(ArgumentAsterismEditDefault)

        // Lookup all of the targets, producing a list of input errors for those
        // that are not found.
        val checks = (tables: Tables) => {
          ed.targetIds.toList.flatMap(_.toList).traverse { id =>
            tables.targets.get(id).toValidNec(
              InputError.missingReference("target", Gid[Target.Id].show(id))
            )
          }.swap.toList.flatMap(_.toNonEmptyList.toList)
        }

        r.editSub(ed.id, ed.editor, checks) { case d: AsterismModel.Default => d }
      }
    )

  def delete[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "deleteAsterism",
      fieldType = AsterismType[F],
      arguments = List(AsterismIdArgument),
      resolve   = c => c.asterism(_.delete(c.asterismId))
    )

  def undelete[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "undeleteAsterism",
      fieldType = AsterismType[F],
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

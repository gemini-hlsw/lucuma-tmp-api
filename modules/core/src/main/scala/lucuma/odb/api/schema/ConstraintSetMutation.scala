// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.ConstraintSetModel
import lucuma.odb.api.repo.OdbRepo

import lucuma.odb.api.schema.syntax.inputtype._

import cats.effect.Effect
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._
import lucuma.odb.api.model.{AirmassRange, ElevationRangeModel, HourAngleRange}

trait ConstraintSetMutation {

  import GeneralSchema.EnumTypeExistence
  import ConstraintSetSchema._
  import ProgramSchema.ProgramIdType
  import context._
  import syntax.inputobjecttype._

  implicit val InputObjectTypeAirmassRangeCreate: InputObjectType[AirmassRange.Create] =
    deriveInputObjectType[AirmassRange.Create](
      InputObjectTypeName("CreateAirmassRangeInput"),
      InputObjectTypeDescription("Airmass range creation parameters")
    )

  implicit val InputObjectTypeHourAngleRangeCreate: InputObjectType[HourAngleRange.Create] =
    deriveInputObjectType[HourAngleRange.Create](
      InputObjectTypeName("CreateHourAngleRangeInput"),
      InputObjectTypeDescription("Hour angle range creation parameters")
    )

  implicit val InputObjectTypeElevationRangeCreate: InputObjectType[ElevationRangeModel.Create] =
    deriveInputObjectType[ElevationRangeModel.Create](
      InputObjectTypeName("CreateElevationRangeInput"),
      InputObjectTypeDescription("Elevation range creation parameters")
    )

  val InputObjectTypeConstraintSetCreate: InputObjectType[ConstraintSetModel.Create] =
    deriveInputObjectType[ConstraintSetModel.Create](
      InputObjectTypeName("CreateConstraintSetInput"),
      InputObjectTypeDescription("Constraint set creation parameters")
    )

  val ArgumentConstraintSetCreate: Argument[ConstraintSetModel.Create] =
    InputObjectTypeConstraintSetCreate.argument(
      "input",
      "Constraint set description"
    )

  val InputObjectTypeConstraintSetEdit: InputObjectType[ConstraintSetModel.Edit] =
    deriveInputObjectType[ConstraintSetModel.Edit](
      InputObjectTypeName("EditConstraintSetInput"),
      InputObjectTypeDescription("Edit constraint set"),
      ReplaceInputField("existence", EnumTypeExistence.notNullableField("existence")),
      ReplaceInputField("name", StringType.notNullableField("name")),
      ReplaceInputField("imageQuality", EnumTypeImageQuality.notNullableField("imageQuality")),
      ReplaceInputField("cloudExtinction",
                        EnumTypeCloudExtinction.notNullableField("cloudExtinction")
      ),
      ReplaceInputField("skyBackground", EnumTypeSkyBackground.notNullableField("skyBackground")),
      ReplaceInputField("waterVapor", EnumTypeWaterVapor.notNullableField("waterVapor")),
      ReplaceInputField("elevationRange",
                        InputObjectTypeElevationRangeCreate.notNullableField("elevationRange")
      )
    )

  val ArgumentConstraintSetEdit: Argument[ConstraintSetModel.Edit] =
    InputObjectTypeConstraintSetEdit.argument(
      "input",
      "Edit constraint set"
    )

  def create[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "createConstraintSet",
      fieldType = OptionType(ConstraintSetType[F]),
      arguments = List(ArgumentConstraintSetCreate),
      resolve   = c => c.constraintSet(_.insert(c.arg(ArgumentConstraintSetCreate)))
    )

  def update[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "updateConstraintSet",
      fieldType = ConstraintSetType[F],
      arguments = List(ArgumentConstraintSetEdit),
      resolve   = c => c.constraintSet(_.edit(c.arg(ArgumentConstraintSetEdit)))
    )

  def delete[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "deleteContraintSet",
      fieldType = ConstraintSetType[F],
      arguments = List(ConstraintSetIdArgument),
      resolve   = c => c.constraintSet(_.delete(c.constraintSetId))
    )

  def undelete[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "undeleteContraintSet",
      fieldType = ConstraintSetType[F],
      arguments = List(ConstraintSetIdArgument),
      resolve   = c => c.constraintSet(_.undelete(c.constraintSetId))
    )

  def allFields[F[_]: Effect]: List[Field[OdbRepo[F], Unit]] =
    List(
      create,
      update,
      delete,
      undelete
    )
}

object ConstraintSetMutation extends ConstraintSetMutation

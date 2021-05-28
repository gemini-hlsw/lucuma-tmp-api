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

  import GeneralSchema.NonEmptyStringType
  import ConstraintSetSchema._
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

  implicit val InputObjectTypeConstraintSetCreate: InputObjectType[ConstraintSetModel.Create] =
    deriveInputObjectType[ConstraintSetModel.Create](
      InputObjectTypeName("CreateConstraintSetInput"),
      InputObjectTypeDescription("Constraint set creation parameters")
    )

  val ArgumentConstraintSetCreate: Argument[ConstraintSetModel.Create] =
    InputObjectTypeConstraintSetCreate.argument(
      "input",
      "Constraint set description"
    )

  implicit val InputObjectTypeConstraintSetEdit: InputObjectType[ConstraintSetModel.Edit] =
    deriveInputObjectType[ConstraintSetModel.Edit](
      InputObjectTypeName("EditConstraintSetInput"),
      InputObjectTypeDescription("Edit constraint set"),
      ReplaceInputField("name", NonEmptyStringType.notNullableField("name")),
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

  def bulkEdit[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "bulkEditConstraintSet",
      fieldType = ConstraintSetType[F],
      arguments = List(ArgumentConstraintSetEdit),
      resolve   = _ => ConstraintSetModel.AnyConstraints
//        c.constraintSet {
//          val e = c.arg(ArgumentConstraintSetEdit)
//          _.edit(e.id, e.editor)
//        }
    )

  def allFields[F[_]: Effect]: List[Field[OdbRepo[F], Unit]] =
    List(bulkEdit[F])
}

object ConstraintSetMutation extends ConstraintSetMutation

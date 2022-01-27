// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{AirmassRangeInput, ConstraintSetModel, ElevationRangeInput, HourAngleRangeInput}
import lucuma.odb.api.schema.syntax.inputtype._
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._

trait ConstraintSetMutation {

  import ConstraintSetSchema._
  import syntax.inputobjecttype._

  implicit val InputObjectTypeAirmassRange: InputObjectType[AirmassRangeInput] =
    deriveInputObjectType[AirmassRangeInput](
      InputObjectTypeName("AirmassRangeInput"),
      InputObjectTypeDescription("Airmass range creation and edit parameters")
    )

  implicit val InputObjectTypeHourAngleRange: InputObjectType[HourAngleRangeInput] =
    deriveInputObjectType[HourAngleRangeInput](
      InputObjectTypeName("CreateHourAngleRangeInput"),
      InputObjectTypeDescription("Hour angle range creation parameters")
    )

  implicit val InputObjectTypeElevationRange: InputObjectType[ElevationRangeInput] =
    InputObjectType[ElevationRangeInput](
      "ElevationRangeInput",
      "Elevation range creation and edit parameters.  Choose one of airmass or hour angle constraints.",
      List(
        InputObjectTypeAirmassRange.optionField("airmassRange"),
        InputObjectTypeHourAngleRange.optionField("hourAngleRange")
      )
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
      ReplaceInputField("imageQuality", EnumTypeImageQuality.notNullableField("imageQuality")),
      ReplaceInputField("cloudExtinction",
                        EnumTypeCloudExtinction.notNullableField("cloudExtinction")
      ),
      ReplaceInputField("skyBackground", EnumTypeSkyBackground.notNullableField("skyBackground")),
      ReplaceInputField("waterVapor", EnumTypeWaterVapor.notNullableField("waterVapor")),
      ReplaceInputField("elevationRange",
                        InputObjectTypeElevationRange.notNullableField("elevationRange")
      )
    )

}

object ConstraintSetMutation extends ConstraintSetMutation

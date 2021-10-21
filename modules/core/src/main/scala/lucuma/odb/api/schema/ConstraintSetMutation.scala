// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{AirmassRange, ConstraintSetModel, ElevationRangeModel, HourAngleRange}
import lucuma.odb.api.schema.syntax.inputtype._
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._

trait ConstraintSetMutation {

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

}

object ConstraintSetMutation extends ConstraintSetMutation

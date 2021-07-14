// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.ConstraintSetModel

import lucuma.odb.api.schema.syntax.inputtype._

import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._
import lucuma.odb.api.model.{AirmassRange, ElevationRangeModel, HourAngleRange}

trait ConstraintSetMutation {

  import GeneralSchema.NonEmptyStringType
  import ConstraintSetSchema._
  import ObservationSchema.ObservationIdType
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

  implicit val InputObjectTypeConstraintSetBulkEdit: InputObjectType[ConstraintSetModel.BulkEdit] =
    deriveInputObjectType[ConstraintSetModel.BulkEdit](
      InputObjectTypeName("BulkEditConstraintSetInput"),
      InputObjectTypeDescription("Bulk edit constraint set of multiple observations")
    )

  val ArgumentConstraintSetBulkEdit: Argument[ConstraintSetModel.BulkEdit] =
    InputObjectTypeConstraintSetBulkEdit.argument(
      "input",
      "Bulk edit constraint set"
    )

}

object ConstraintSetMutation extends ConstraintSetMutation

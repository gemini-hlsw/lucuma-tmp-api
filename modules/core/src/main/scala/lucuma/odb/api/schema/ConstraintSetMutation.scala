// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{AirMassRangeInput, ConstraintSetInput, ElevationRangeInput, HourAngleRangeInput}
import lucuma.odb.api.schema.syntax.inputtype._
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._

trait ConstraintSetMutation {

  import ConstraintSetSchema._
  import RefinedSchema.PosBigDecimalType
  import syntax.inputobjecttype._

  implicit val InputObjectTypeAirmassRange: InputObjectType[AirMassRangeInput] =
    deriveInputObjectType[AirMassRangeInput](
      InputObjectTypeName("AirMassRangeInput"),
      InputObjectTypeDescription("Air mass range creation and edit parameters")
    )

  implicit val InputObjectTypeHourAngleRange: InputObjectType[HourAngleRangeInput] =
    deriveInputObjectType[HourAngleRangeInput](
      InputObjectTypeName("HourAngleRangeInput"),
      InputObjectTypeDescription("Hour angle range creation parameters")
    )

  implicit val InputObjectTypeElevationRange: InputObjectType[ElevationRangeInput] =
    InputObjectType[ElevationRangeInput](
      "ElevationRangeInput",
      "Elevation range creation and edit parameters.  Choose one of airMass or hourAngle constraints.",
      List(
        InputObjectTypeAirmassRange.optionField("airMass"),
        InputObjectTypeHourAngleRange.optionField("hourAngle")
      )
    )

  implicit val InputObjectTypeConstraintSet: InputObjectType[ConstraintSetInput] =
    InputObjectType[ConstraintSetInput](
      "ConstraintSetInput",
      "Constraint set creation and editing parameters",
      List(
        EnumTypeImageQuality.createRequiredEditOptional("imageQuality", "ConstraintSet"),
        EnumTypeCloudExtinction.createRequiredEditOptional("cloudExtinction", "ConstraintSet"),
        EnumTypeSkyBackground.createRequiredEditOptional("skyBackground", "ConstraintSet"),
        EnumTypeWaterVapor.createRequiredEditOptional("waterVapor", "ConstraintSet"),
        InputObjectTypeElevationRange.createRequiredEditOptional("elevationRange", "ConstraintSet")
      )
    )

  val ArgumentConstraintSetInput: Argument[ConstraintSetInput] =
    InputObjectTypeConstraintSet.argument(
      "input",
      "Constraint set description"
    )


}

object ConstraintSetMutation extends ConstraintSetMutation

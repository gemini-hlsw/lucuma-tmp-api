// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import eu.timepit.refined.types.all.PosBigDecimal
import lucuma.core.enums.{CloudExtinction, ImageQuality, SkyBackground, WaterVapor}
import lucuma.core.model.{ConstraintSet, ElevationRange}
import lucuma.odb.api.schema.syntax.all._
import sangria.schema._

object ConstraintSetSchema {

  import RefinedSchema.PosBigDecimalType

  implicit val EnumTypeCloudExtinction: EnumType[CloudExtinction] =
    EnumType.fromEnumerated("CloudExtinction", "Cloud extinction")

  implicit val EnumTypeImageQuality: EnumType[ImageQuality] =
    EnumType.fromEnumerated("ImageQuality", "Image quality")

  implicit val EnumTypeSkyBackground: EnumType[SkyBackground] =
    EnumType.fromEnumerated("SkyBackground", "Sky background")

  implicit val EnumTypeWaterVapor: EnumType[WaterVapor] =
    EnumType.fromEnumerated("WaterVapor", "Water vapor")

  val AirMassRangeType: ObjectType[Any, ElevationRange.AirMass] =
    ObjectType(
      name     = "AirMassRange",
      fieldsFn = () =>
        fields(
          Field(
            name        = "min",
            fieldType   = PosBigDecimalType,
            description = Some("Minimum AirMass (unitless)"),
            resolve     = c => PosBigDecimal.unsafeFrom(c.value.min.value)
          ),
          Field(
            name        = "max",
            fieldType   = PosBigDecimalType,
            description = Some("Maximum AirMass (unitless)"),
            resolve     = c => PosBigDecimal.unsafeFrom(c.value.max.value)
          )
        )
    )

  val HourAngleRangeType: ObjectType[Any, ElevationRange.HourAngle] =
    ObjectType(
      name     = "HourAngleRange",
      fieldsFn = () =>
        fields(
          Field(
            name        = "minHours",
            fieldType   = BigDecimalType,
            description = Some("Minimum Hour Angle (hours)"),
            resolve     = _.value.minHours.value
          ),
          Field(
            name        = "maxHours",
            fieldType   = BigDecimalType,
            description = Some("Maximum Hour Angle (hours)"),
            resolve     = _.value.maxHours.value
          )
        )
    )

  val ElevationRangeModelType: ObjectType[Any, ElevationRange] =
    ObjectType(
      name        = "ElevationRange",
      description = "Either air mass range or elevation range",
      fieldsFn    = () =>
        fields(
          Field(
            name        = "airMass",
            fieldType   = OptionType(AirMassRangeType),
            description = Some("AirMass range if elevation range is an Airmass range"),
            resolve     = c => ElevationRange.airMass.getOption(c.value)
          ),
          Field(
            name        = "hourAngle",
            fieldType   = OptionType(HourAngleRangeType),
            description = Some("Hour angle range if elevation range is an Hour angle range"),
            resolve     = c => ElevationRange.hourAngle.getOption(c.value)
          )
        )
    )

  val ConstraintSetType: ObjectType[Any, ConstraintSet] =
    ObjectType(
      name     = "ConstraintSet",
      fieldsFn = () =>
        fields(
          Field(
            name        = "imageQuality",
            fieldType   = EnumTypeImageQuality,
            description = Some("Image quality"),
            resolve     = _.value.imageQuality
          ),
          Field(
            name        = "cloudExtinction",
            fieldType   = EnumTypeCloudExtinction,
            description = Some("Cloud extinction"),
            resolve     = _.value.cloudExtinction
          ),
          Field(
            name        = "skyBackground",
            fieldType   = EnumTypeSkyBackground,
            description = Some("Sky background"),
            resolve     = _.value.skyBackground
          ),
          Field(
            name        = "waterVapor",
            fieldType   = EnumTypeWaterVapor,
            description = Some("Water vapor"),
            resolve     = _.value.waterVapor
          ),
          Field(
            name        = "elevationRange",
            fieldType   = ElevationRangeModelType,
            description = Some("Either air mass range or elevation range"),
            resolve     = _.value.elevationRange
          )
        )
    )

}

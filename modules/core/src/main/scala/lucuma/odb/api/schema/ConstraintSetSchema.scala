// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.enum.{CloudExtinction, ImageQuality, SkyBackground, WaterVapor}
import lucuma.odb.api.model.{AirmassRange, ElevationRangeModel, HourAngleRange}
import lucuma.odb.api.repo.OdbRepo
import lucuma.odb.api.schema.syntax.all._
import lucuma.odb.api.model.ConstraintSetModel

import sangria.schema._

object ConstraintSetSchema {

  implicit val EnumTypeCloudExtinction: EnumType[CloudExtinction] =
    EnumType.fromEnumerated("CloudExtinction", "Cloud extinction")

  implicit val EnumTypeImageQuality: EnumType[ImageQuality] =
    EnumType.fromEnumerated("ImageQuality", "Image quality")

  implicit val EnumTypeSkyBackground: EnumType[SkyBackground] =
    EnumType.fromEnumerated("SkyBackground", "Sky background")

  implicit val EnumTypeWaterVapor: EnumType[WaterVapor] =
    EnumType.fromEnumerated("WaterVapor", "Water vapor")

  def AirMassRangeType[F[_]]: ObjectType[OdbRepo[F], AirmassRange] =
    ObjectType(
      name     = "AirMassRange",
      fieldsFn = () =>
        fields(
          Field(
            name        = "min",
            fieldType   = BigDecimalType,
            description = Some("Minimum Airmass (unitless)"),
            resolve     = _.value.min.value
          ),
          Field(
            name        = "max",
            fieldType   = BigDecimalType,
            description = Some("Maximum Airmass (unitless)"),
            resolve     = _.value.max.value
          )
        )
    )

  def HourAngleRangeType[F[_]]: ObjectType[OdbRepo[F], HourAngleRange] =
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

  def ElevationRangeModelType[F[_]]: OutputType[ElevationRangeModel] =
    UnionType(
      name        = "ElevationRange",
      description = Some("Either airmass range or elevation range"),
      types       = List(AirMassRangeType[F], HourAngleRangeType[F])
    ).mapValue[ElevationRangeModel](identity)

  def ConstraintSetType[F[_]]: ObjectType[OdbRepo[F], ConstraintSetModel] =
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
            description = Some("Either airmass range or elevation range"),
            resolve     = _.value.elevationRange
          ),
          Field(
            name        = "airmassRange",
            fieldType   = OptionType(AirMassRangeType[F]),
            description = Some("Airmass range if elevation range is an Airmass range"),
            resolve     = c => ElevationRangeModel.airmassRange.getOption(c.value.elevationRange)
          ),
          Field(
            name        = "hourAngleRange",
            fieldType   = OptionType(HourAngleRangeType[F]),
            description = Some("Hour angle range if elevation range is an Hour angle range"),
            resolve     = c => ElevationRangeModel.hourAngleRange.getOption(c.value.elevationRange)
          )
        )
    )

}

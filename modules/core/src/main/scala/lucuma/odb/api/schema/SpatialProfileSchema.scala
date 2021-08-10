// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.enum.SpatialProfileType
import lucuma.odb.api.model.SpatialProfileModel
import lucuma.odb.api.schema.syntax.`enum`._
import sangria.schema._
import sangria.macros.derive._

object SpatialProfileSchema {
  import NumericUnitsSchema._

  implicit val EnumSpatialProfileModelUnits: EnumType[SpatialProfileModel.Units] =
    EnumType.fromEnumerated(
      "SpatialProfileUnits",
      "Spatial profial gaussian units"
    )

  implicit val InputGaussianSourceAngleInput: InputType[SpatialProfileModel.GaussianSourceAngleInput] =
    deriveInputObjectType[SpatialProfileModel.GaussianSourceAngleInput](
      InputObjectTypeName("GaussianSourceAngleInput"),
      InputObjectTypeDescription("Gaussian source angle in appropriate units"),
    )

  implicit val EnumSpatialProfileType: EnumType[SpatialProfileType] =
    EnumType.fromEnumerated(
      "SpatialProfileType",
      "Spatial profile type: Point/Uniform/Gaussian"
    )

  implicit val InputSpatialProfileModelInput: InputObjectType[SpatialProfileModel.Input] =
    deriveInputObjectType[SpatialProfileModel.Input](
      InputObjectTypeName("SpatialProfileModelInput"),
      InputObjectTypeDescription("Spatial profile PointSource/UniformSource/GaussianSource"),
      DocumentInputField("sourceType", "Spatial profile type: Point/Uniform/Gaussian"),
      DocumentInputField("fwhm", "Full width half maximum (including seeing) in suitable units. Required for the Gaussian spatial profile, ignored otherwise."),
    )

}

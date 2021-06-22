// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import sangria.schema._
import sangria.macros.derive._
import sangria.marshalling.circe._
import lucuma.odb.api.model.ConfigurationAlternativesModel

object ConfigurationAlternativesSchema {
  import WavelengthSchema._
  import RefinedSchema._
  import TargetMutation.InputObjectMagnitude
  import SpatialProfileSchema._
  import SpectralDistributionSchema._
  import syntax.inputobjecttype._

  val InputConfigurationAlternativesModelSearch: InputObjectType[ConfigurationAlternativesModel.Search] =
    deriveInputObjectType[ConfigurationAlternativesModel.Search](
      InputObjectTypeName("QueryConfigurationAlternativeSearchInput"),
      InputObjectTypeDescription("Configuration alternatives query"),
      DocumentInputField("wavelength", description  = "Observing wavelength."),
      DocumentInputField("simultaneousCoverage", description  = "Minimum desired simultaneous wavelength coverage."),
      DocumentInputField("resolution", description  = "Minimum desired resolution."),
      DocumentInputField("signalToNoise", description  = "Minimum desired signal-to-noise ratio."),
      DocumentInputField("spatialProfile", description  = "Spatial profile PointSource/UniformSource/GaussianSource."),
      DocumentInputField("spectralDistribution", description  = "Spectral distribution variant BlacBode/PowerLaw/Stellar/NonStellar."),
      DocumentInputField("magnitude", description  = "Target magnitude/system/band."),
      DocumentInputField("redshift", description  = "Target redshift.")
    )

  val ArgumentConfigurationAlternativesModelSearch: Argument[ConfigurationAlternativesModel.Search] =
    InputConfigurationAlternativesModelSearch.argument(
      "input",
      "Configuraton alternatives search parameters."
    )

}

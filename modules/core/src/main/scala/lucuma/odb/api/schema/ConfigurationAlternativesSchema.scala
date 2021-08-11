// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import sangria.schema._
import sangria.macros.derive._
import sangria.marshalling.circe._
import lucuma.odb.api.model.ConfigurationAlternativesModel
import lucuma.odb.search.SpectroscopyResults
import lucuma.odb.search.Result
import lucuma.odb.api.repo.OdbRepo
import lucuma.odb.search.ObservingMode

object ConfigurationAlternativesSchema {
  import WavelengthSchema._
  import RefinedSchema._
  import TargetMutation.InputObjectMagnitudeCreate
  import SpatialProfileSchema._
  import SpectralDistributionSchema._
  import syntax.inputobjecttype._

  val InputConfigurationAlternativesModelSearch: InputObjectType[ConfigurationAlternativesModel.SearchParameters] =
    deriveInputObjectType[ConfigurationAlternativesModel.SearchParameters](
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

  val ArgumentConfigurationAlternativesModelSearch: Argument[ConfigurationAlternativesModel.SearchParameters] =
    InputConfigurationAlternativesModelSearch.argument(
      "input",
      "Configuraton alternatives search parameters."
    )

  def ObservingModeSpectroscopyType[F[_]]: ObjectType[OdbRepo[F], ObservingMode.Spectroscopy] =
    ObjectType(
      name     = "ObservingModeSpectroscopy",
      fieldsFn = () => fields(

        Field(
          name        = "wavelength",
          fieldType   = WavelengthType[F],
          description = Some("Wavelength in appropriate units"),
          resolve     = _.value.λ
        ),

      )
    )

  def ResultSpectroscopyType[F[_]]: ObjectType[OdbRepo[F], Result.Spectroscopy] =
    ObjectType(
      name     = "Result",
      fieldsFn = () => fields(

        Field(
          name        = "mode",
          fieldType   = ObservingModeSpectroscopyType[F],
          description = Some("Spectroscopy mode"),
          resolve     = _.value.mode
        ),

      )
    )

  def SpectroscopyResultsType[F[_]]: ObjectType[OdbRepo[F], SpectroscopyResults]=
    ObjectType(
      name     = "spectroscopyResult",
      fieldsFn = () => fields(

        Field(
          name        = "results",
          fieldType   = ListType(ResultSpectroscopyType[F]),
          description = Some("Search results"),
          resolve     = _.value.results
        ),

      )
    )
}

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.refined._
import eu.timepit.refined.types.numeric.PosInt

object ConfigurationAlternativesModel {
  final case class Search(
    wavelength: Option[WavelengthModel.Input],
    simultaneousCoverage: Option[WavelengthModel.Input],
    resolution: Option[PosInt],
    signalToNoise: Option[PosInt],
    spatialProfile: Option[SpatialProfileModel.Input],
    spectralDistribution: Option[SpectralDistributionModel.Input],
    magnitude: Option[MagnitudeModel.Input],
    redshift: Option[BigDecimal]
)
  object Search {

    implicit val DecoderSerch: Decoder[Search] =
      deriveDecoder[Search]

  }
}

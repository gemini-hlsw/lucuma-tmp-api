// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import eu.timepit.refined.types.numeric.PosInt
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.refined._
import lucuma.core.enum.SpectralDistributionType

object ConfigurationAlternativesModel {
  final case class Search(
    wavelength: Option[WavelengthModel.Input],
    simultaneousCoverage: Option[WavelengthModel.Input],
    resolution: Option[PosInt],
    signalToNoise: Option[PosInt],
    spatialProfile: Option[SpatialProfileModel.Input],
    spectralDistribution: Option[SpectralDistributionType],
    magnitude: Option[MagnitudeModel.Create],
    redshift: Option[BigDecimal]
)
  object Search {

    implicit val DecoderSerch: Decoder[Search] =
      deriveDecoder[Search]

  }
}

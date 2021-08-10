// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import eu.timepit.refined.types.numeric.PosInt
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.refined._
import lucuma.core.enum.SpectralDistributionType

object ConfigurationAlternativesModel {
  final case class SearchParameters(
    wavelength: WavelengthModel.Input,
    simultaneousCoverage: WavelengthModel.Input,
    resolution: PosInt,
    signalToNoise: PosInt,
    spatialProfile: SpatialProfileModel.Input,
    spectralDistribution: SpectralDistributionType,
    magnitude: MagnitudeModel.Create,
    redshift: BigDecimal
)
  object SearchParameters {

    implicit val DecoderSerch: Decoder[SearchParameters] =
      deriveDecoder[SearchParameters]

  }
}

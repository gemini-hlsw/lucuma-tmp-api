// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.SpectralDistributionModel
import lucuma.odb.api.schema.syntax.`enum`._
import sangria.schema._

object SpectralDistributionSchema {

  implicit val EnumSpectralDistributionModelInput: EnumType[SpectralDistributionModel.Input] =
    EnumType.fromEnumerated(
      "SpctralDistribution",
      "Spectral distribution variant"
    )

}

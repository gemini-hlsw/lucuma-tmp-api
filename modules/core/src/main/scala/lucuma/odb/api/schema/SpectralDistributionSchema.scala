// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.enum.SpectralDistributionType
import lucuma.odb.api.schema.syntax.`enum`._
import sangria.schema._

object SpectralDistributionSchema {

  implicit val EnumSpectralDistributionModelInput: EnumType[SpectralDistributionType] =
    EnumType.fromEnumerated(
      "SpctralDistribution",
      "Spectral distribution variant"
    )

}

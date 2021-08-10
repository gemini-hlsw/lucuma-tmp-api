// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.search

import lucuma.core.enum._
import lucuma.core.math.Redshift

/** Target properties we need to know at phase zero. */
final case class TargetProfile(
  spatialProfile:       SpatialProfileType,
  spectralDistribution: SpectralDistributionType,
  magnitude:            Double,
  magnitudeSystem:      MagnitudeSystem,
  magnitudeBand:        MagnitudeBand,
  redshift:             Redshift
)



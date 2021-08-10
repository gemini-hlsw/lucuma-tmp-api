// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.search

import lucuma.core.enum._
import lucuma.core.math.Redshift
import lucuma.core.model.Magnitude
import lucuma.odb.api.model.SpatialProfile

/** Target properties we need to know at phase zero. */
final case class TargetProfile(
  spatialProfile:       SpatialProfile,
  spectralDistribution: SpectralDistributionType,
  magnitude:            Magnitude,
  redshift:             Redshift
)



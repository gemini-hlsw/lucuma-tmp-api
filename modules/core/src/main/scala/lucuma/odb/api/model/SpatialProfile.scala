// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.math.Angle

sealed trait SpatialProfile extends Product with Serializable

object SpatialProfile {
  final case object PointSource    extends SpatialProfile
  final case object UniformSource  extends SpatialProfile
  final case class GaussianSource(fwhm: Angle) extends SpatialProfile
}


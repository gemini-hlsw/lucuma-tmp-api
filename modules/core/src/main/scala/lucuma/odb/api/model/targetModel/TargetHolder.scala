// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import lucuma.core.model.{EphemerisKey, NonsiderealTarget, SiderealTarget, SiderealTracking, Target}

/**
 * An interface for objects that contain a `lucuma.core.model.Target`.
 */
trait TargetHolder {

  def target: Target

  def track: Either[EphemerisKey, SiderealTracking] =
    target match {
      case NonsiderealTarget(_, k, _, _) => Left(k)
      case SiderealTarget(_, t, _, _)    => Right(t)
    }

}

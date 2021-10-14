// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import lucuma.core.model.Target

/**
 * An interface for objects that contain a `lucuma.core.model.Target`.
 */
trait TargetHolder {

  def target: Target

}

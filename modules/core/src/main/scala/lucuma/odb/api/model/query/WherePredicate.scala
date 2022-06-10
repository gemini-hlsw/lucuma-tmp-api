// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.query

import scala.annotation.nowarn

trait WherePredicate[A] {

  @nowarn
  def matches(a: A): Boolean =
    true

}

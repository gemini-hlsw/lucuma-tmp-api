// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.model.WithId

import eu.timepit.refined.auto._

// To be moved to lucuma.core.model.ids, presumably

object Atom extends WithId {
  protected val idTag = 'm'
}

object Step extends WithId {
  protected val idTag = 's'
}



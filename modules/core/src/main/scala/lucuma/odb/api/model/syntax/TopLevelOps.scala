// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.syntax

import lucuma.odb.api.model.TopLevel

final class TopLevelOps[I, T: TopLevel[I, ?]](self: T) {

  def isPresent: Boolean =
    TopLevel[I, T].existence.get(self).isPresent

  def isDeleted: Boolean =
    TopLevel[I, T].existence.get(self).isDeleted

}

trait ToTopLevelOps {
  implicit def toTopLevelOps[I, T: TopLevel[I, ?]](t: T): TopLevelOps[I, T] =
    new TopLevelOps[I, T](t)
}

object toplevel extends ToTopLevelOps

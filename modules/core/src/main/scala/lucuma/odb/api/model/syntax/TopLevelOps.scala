// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.syntax

import lucuma.odb.api.model.TopLevelModel

final class TopLevelOps[I, T: TopLevelModel[I, *]](self: T) {

  def isPresent: Boolean =
    TopLevelModel[I, T].existence.get(self).isPresent

  def isDeleted: Boolean =
    TopLevelModel[I, T].existence.get(self).isDeleted

}

trait ToTopLevelOps {
  implicit def toTopLevelOps[I, T: TopLevelModel[I, *]](t: T): TopLevelOps[I, T] =
    new TopLevelOps[I, T](t)
}

object toplevel extends ToTopLevelOps

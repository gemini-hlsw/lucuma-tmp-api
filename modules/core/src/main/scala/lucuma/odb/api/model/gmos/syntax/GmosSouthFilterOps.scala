// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.gmos.syntax

import lucuma.core.`enum`.GmosSouthFilter
import lucuma.core.`enum`.GmosSouthFilter.{GPrime, IPrime, RPrime, UPrime, ZPrime}

final class GmosSouthFilterCompanionOps(val self: GmosSouthFilter.type) extends AnyVal {

  def allAcquisition: List[GmosSouthFilter] =
    List(UPrime, GPrime, RPrime, IPrime, ZPrime)

}

trait ToGmosSouthFilterCompanionOps {
  implicit def toGmosSouthFilterCompanionOps(self: GmosSouthFilter.type): GmosSouthFilterCompanionOps =
    new GmosSouthFilterCompanionOps(self)
}

object gmosSouthFilter extends ToGmosSouthFilterCompanionOps

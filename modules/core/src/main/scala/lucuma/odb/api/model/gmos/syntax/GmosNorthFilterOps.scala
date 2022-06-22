// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.gmos.syntax

import cats.data.NonEmptyList
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFilter.{GPrime, IPrime, RPrime, UPrime, ZPrime}

final class GmosNorthFilterCompanionOps(val self: GmosNorthFilter.type) extends AnyVal {

  def allAcquisition: NonEmptyList[GmosNorthFilter] =
    NonEmptyList.of(UPrime, GPrime, RPrime, IPrime, ZPrime)

}

trait ToGmosNorthFilterCompanionOps {
  implicit def toGmosNorthFilterCompanionOps(self: GmosNorthFilter.type): GmosNorthFilterCompanionOps =
    new GmosNorthFilterCompanionOps(self)
}

object gmosNorthFilter extends ToGmosNorthFilterCompanionOps


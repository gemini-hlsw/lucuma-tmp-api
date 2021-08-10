// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.search.syntax

import lucuma.odb.search.Coverage
import cats.implicits._
import lucuma.core.enum.GmosNorthFilter
import lucuma.core.enum.GmosNorthFilter._
import lucuma.core.math.Wavelength

final class GmosNorthFilterOps(val self: GmosNorthFilter) extends AnyVal {

  // see http://www.gemini.edu/node/10621
  def coverage: Coverage = {
    import Wavelength.fromPicometers

    def cov(a: Int, b: Int = Int.MaxValue): Coverage =
      (fromPicometers.getOption(a), fromPicometers.getOption(b))
        .mapN(Coverage.apply)
        .getOrElse(sys.error("Invalid constant coverage."))

    self match {

      // Broad Band Imaging Filters
      case UPrime           => cov(336000,  385000)
      case GPrime           => cov(398000,  552000)
      case RPrime           => cov(562000,  698000)
      case IPrime           => cov(706000,  850000)
      case CaT              => cov(780000,  993000)
      case ZPrime           => cov(848000)
      case Z                => cov(830000,  925000)
      case Y                => cov(970000, 1070000)
      // ri -- not in OCS3 … need to add

      // Narrow Band Imaging Filters
      case HeII             => cov(464000,  472000)
      case HeIIC            => cov(474000,  482000)
      case OIII             => cov(496500,  501500)
      case OIIIC            => cov(509000,  519000)
      case Ha               => cov(654000,  661000)
      case HaC              => cov(659000,  665000)
      case SII              => cov(669400,  673700)
      // OVIC? OVI? -- these aren't in OCS3 … need to add
      case DS920            => cov(912800,  931400)

      // Spectroscopy Blocking Filters
      case GG455            => cov(460000)
      case OG515            => cov(520000)
      case RG610            => cov(615000)

      // These are only used for engineering and will never be selected by search algorithm, but
      // we still need to handle them. For now we'll just pretend they have no coverage at all.
      case HartmannA_RPrime => Coverage.Empty
      case HartmannB_RPrime => Coverage.Empty

      // Allowed Filter Combinations
      case GPrime_GG455     => cov(460000,  552000)
      case GPrime_OG515     => cov(520000,  552000)
      case RPrime_RG610     => cov(615000,  698000)
      case IPrime_CaT       => cov(780000,  850000)
      case ZPrime_CaT       => cov(848000,  933000)
    }
  }

}


trait ToGmosNorthFilterOps {
  implicit def toGmosNorthFilterOps(self: GmosNorthFilter): GmosNorthFilterOps =
    new GmosNorthFilterOps(self)
}


final class GmosNorthFilterCompanionOps(val self: GmosNorthFilter.type) extends AnyVal {

  /** Filters to use for acquisition, sorted by wavelength. */
  def allAcquisition: List[GmosNorthFilter] =
    List(UPrime, GPrime, RPrime, IPrime, ZPrime).sortBy(_.wavelength)

}

trait ToGmosNorthFilterCompanionOps {
  implicit def toGmosNorthFilterCompanionOps(self: GmosNorthFilter.type): GmosNorthFilterCompanionOps =
    new GmosNorthFilterCompanionOps(self)
}

object gmosnorthfilter
  extends ToGmosNorthFilterOps
     with ToGmosNorthFilterCompanionOps

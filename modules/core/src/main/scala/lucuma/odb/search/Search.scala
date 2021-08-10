// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.search

import cats._
import cats.syntax.all._
import lucuma.core.enum._
import lucuma.odb.itc.Itc
import lucuma.odb.search.gmosnorth.GmosNorthFilterSelector

object Search {

  sealed trait Result {
    def mode: ObservingMode
    def itc:  Itc.Result
  }

  object Result {

    case class Spectroscopy(mode: ObservingMode.Spectroscopy, itc: Itc.Result)

  }


  def spectroscopy[F[_]: Parallel: Monad: Itc](
    constraints:   Constraints.Spectroscopy,
    targetProfile: TargetProfile,
    signalToNoise: Int
  ): F[List[Result.Spectroscopy]] = {

    // As a first pass we'll generate every possible configuration and then filter them at the end.
    // This lets us apply the constraints in one place rather than duplicating the filtering logic
    // for each instrument (at the cost of dealing with some large sets in memory).

    val excludedFPUs: Set[GmosNorthFpu] = {
      import GmosNorthFpu._
      Set(Ifu1, Ifu2, Ifu3, Ns0, Ns1, Ns2, Ns3, Ns4, Ns5)
    }

    val gmosNorthModes: List[ObservingMode.Spectroscopy] =
      for {
        disp   <- GmosNorthDisperser.all
        fpu    <- GmosNorthFpu.all.filterNot(excludedFPUs)
        filter <- GmosNorthFilterSelector.selectBlocking(disp, fpu, constraints.λ).toList
      } yield ObservingMode.Spectroscopy.GmosNorth(constraints.λ, disp, fpu, filter)

    // more instruments ...

    // Every spectrographic observing mode
    val allModes: List[ObservingMode.Spectroscopy] =
      gmosNorthModes // ++ ...

    // Now filter down the list.
    val compatibleModes: List[ObservingMode.Spectroscopy] =
      allModes
        .filter(_.coverage.width >= constraints.simultaneousCoverage)
        .filter(_.resolution     >= constraints.resolution)

    // Done!
    compatibleModes.parTraverse { mode =>
      Itc[F].calculate(targetProfile, mode, signalToNoise).map(Result.Spectroscopy(mode, _))
    } .map(_.sortBy {
      case Result.Spectroscopy(_, Itc.Result.Success(t, n, _)) => t.toSeconds.toDouble * n
      case Result.Spectroscopy(_, Itc.Result.SourceTooBright)  => Double.MaxValue
    })

  }

}

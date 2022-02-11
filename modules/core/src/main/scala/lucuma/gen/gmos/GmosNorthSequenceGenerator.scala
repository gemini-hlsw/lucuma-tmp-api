// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen.gmos

import lucuma.core.`enum`.{GmosAmpCount, GmosAmpGain, GmosAmpReadMode, GmosDtax, GmosRoi, GmosXBinning, GmosYBinning}
import lucuma.odb.api.model.GmosModel.{CcdReadout, NorthDynamic}

import scala.concurrent.duration._

trait GmosNorthSequenceGenerator {

  val initialConfig: NorthDynamic =
    NorthDynamic(
      exposure = 0.seconds,
      readout  = CcdReadout(
        GmosXBinning.One,
        GmosYBinning.One,
        GmosAmpCount.Twelve,
        GmosAmpGain.Low,
        GmosAmpReadMode.Fast
      ),
      dtax    = GmosDtax.Zero,
      roi     = GmosRoi.FullFrame,
      grating = None,
      filter  = None,
      fpu     = None
    )

  /**
   * Produces a "science" step based upon the current instrument configuration
   * state and the given telescope configuration.
   *
   * @param t telescope config to associate with the step
   */
//  def scienceStep(t: TelescopeConfig): State[GmosN, Step.GmosN] =
//    State.inspect(Step.GmosN(_, Step.Base.Science(t)))

  /**
   * Produces a "science" step based upon the current instrument configuration
   * state and the given offset.
   *
   * @param p offset in p
   * @param q offset in q
   */
//  def scienceStep(p: Angle, q: Angle): State[GmosN, Step.GmosN] =
//    scienceStep(TelescopeConfig(Offset.P(p), Offset.Q(q)))


}

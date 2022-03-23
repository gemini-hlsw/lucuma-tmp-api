// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen
package gmos

import lucuma.core.`enum`.{GmosAmpCount, GmosAmpGain, GmosAmpReadMode, GmosDtax, GmosRoi, GmosXBinning, GmosYBinning}
import lucuma.odb.api.model.GmosModel.{CcdReadout, NorthDynamic}

import scala.concurrent.duration._

private[gmos] trait GmosNorthSequenceGenerator extends SequenceGenerator[NorthDynamic] {

  override val initialConfig: NorthDynamic =
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

}

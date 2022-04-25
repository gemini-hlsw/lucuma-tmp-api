// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import coulomb.Quantity
import eu.timepit.refined.types.all.{NonEmptyString, PosDouble}
import lucuma.core.`enum`.{GmosAmpGain, GmosAmpReadMode, GmosNorthDisperser, GmosNorthFilter, GmosNorthFpu, GmosRoi, GmosXBinning, GmosYBinning, ImageQuality, Instrument}
import lucuma.core.math.{Angle, Offset}
import lucuma.core.math.units.Nanometer
import lucuma.core.model.SourceProfile

sealed trait ScienceMode extends Product with Serializable {

  def instrument: Instrument

}

object ScienceMode {

  /**
   * GmosNorthLongSlit mode. BasicConfig options can be overridden or expanded
   * upon in AdvancedConfig if desired.  The AdvancedConfig serves as the input
   * to sequence generation.
   */
  final case class GmosNorthLongSlit(
    basic:    GmosNorthLongSlit.BasicConfig,
    advanced: Option[GmosNorthLongSlit.AdvancedConfig]
  ) extends ScienceMode {

    override def instrument: Instrument =
      Instrument.GmosNorth

  }

  object GmosNorthLongSlit {

    /**
     * BasicConfig options that match the science requirements are listed in a
     * table in Explore.  The user selects one, but may subsequently choose to
     * override (or refine details) in an AdvancedConfig without losing any
     * information in the original BasicConfig.
     */
    final case class BasicConfig(
      grating:   GmosNorthDisperser,
      filter:    Option[GmosNorthFilter],
      fpu:       GmosNorthFpu
    ) {

      def toAdvanced: AdvancedConfig =
        AdvancedConfig(
          name          = None,
          overrideBasic = this
        )

    }

    /**
     * AdvancedConfig options provide more control over the sequence that is
     * generated without resorting to a manual sequence.
     */
    final case class AdvancedConfig(
      name:                   Option[NonEmptyString],
      overrideBasic:          BasicConfig,
      explicitXBin:           Option[GmosXBinning]                   = None,  // calculated from effective slit and sampling by default
      explicitYBin:           Option[GmosYBinning]                   = None,
      explicitAmpReadMode:    Option[GmosAmpReadMode]                = None,
      explicitAmpGain:        Option[GmosAmpGain]                    = None,
      explicitRoi:            Option[GmosRoi]                        = None,
      explicitλDithers:       Option[List[Quantity[Int, Nanometer]]] = None,
      explicitSpatialOffsets: Option[List[Offset.Q]]                 = None
    ) {

      import lucuma.gen.gmos.longslit.syntax.gmosNorthGrating._
      import lucuma.gen.gmos.longslit.syntax.gmosNorthFpu._

      def grating: GmosNorthDisperser =
        overrideBasic.grating

      def filter: Option[GmosNorthFilter] =
        overrideBasic.filter

      def fpu: GmosNorthFpu =
        overrideBasic.fpu

      def xBin(sourceProfile: SourceProfile, imageQuality: ImageQuality, sampling: PosDouble): GmosXBinning =
        explicitXBin.getOrElse(fpu.xbin(sourceProfile, imageQuality, sampling))

      def yBin: GmosYBinning =
        explicitYBin.getOrElse(AdvancedConfig.DefaultYBinning)

      def ampReadMode: GmosAmpReadMode =
        explicitAmpReadMode.getOrElse(AdvancedConfig.DefaultAmpReadMode)

      def ampGain: GmosAmpGain =
        explicitAmpGain.getOrElse(AdvancedConfig.DefaultAmpGain)

      def roi: GmosRoi =
        explicitRoi.getOrElse(AdvancedConfig.DefaultRoi)

      def λDithers: List[Quantity[Int, Nanometer]] =
        explicitλDithers.getOrElse {
          List(
            Quantity[Int, Nanometer](0),
            overrideBasic.grating.Δλ
          )
        }

      def spatialOffsets: List[Offset.Q] =
        explicitSpatialOffsets.getOrElse(AdvancedConfig.DefaultSpatialOffsets)

    }

    object AdvancedConfig {

      val DefaultYBinning: GmosYBinning =
        GmosYBinning.Two

      val DefaultAmpReadMode: GmosAmpReadMode =
        GmosAmpReadMode.Slow

      val DefaultAmpGain: GmosAmpGain =
        GmosAmpGain.Low

      val DefaultRoi: GmosRoi =
        GmosRoi.FullFrame

      val DefaultSpatialOffsets: List[Offset.Q] =
        List(Offset.Q.Zero, Offset.Q(Angle.arcseconds.reverseGet(15)))

    }

  }

}

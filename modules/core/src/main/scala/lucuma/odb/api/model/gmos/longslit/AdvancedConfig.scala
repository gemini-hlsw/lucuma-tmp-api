// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.gmos.longslit

import cats.Eq
import cats.data.NonEmptyList
import coulomb.Quantity
import coulomb.cats.implicits._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.all.NonEmptyString
import lucuma.core.enums.{GmosAmpGain, GmosAmpReadMode, GmosRoi, GmosXBinning, GmosYBinning}
import lucuma.core.math.{Angle, Offset, Wavelength}
import lucuma.core.math.units.Nanometer
import lucuma.core.model.ExposureTimeMode
import monocle.{Focus, Lens}

/**
 * AdvancedConfig options provide more control over the sequence that is
 * generated without resorting to a manual sequence.
 */
final case class AdvancedConfig[G, F, U](
  name:                     Option[NonEmptyString],
  overrideWavelength:       Option[Wavelength]                                    = None,
  overrideGrating:          Option[G]                                             = None,
  overrideFilter:           Option[Option[F]]                                     = None,
  overrideFpu:              Option[U]                                             = None,
  overrideExposureTimeMode: Option[ExposureTimeMode]                              = None,
  explicitXBin:             Option[GmosXBinning]                                  = None,  // calculated from effective slit and sampling by default
  explicitYBin:             Option[GmosYBinning]                                  = None,
  explicitAmpReadMode:      Option[GmosAmpReadMode]                               = None,
  explicitAmpGain:          Option[GmosAmpGain]                                   = None,
  explicitRoi:              Option[GmosRoi]                                       = None,
  explicitλDithers:         Option[NonEmptyList[Quantity[BigDecimal, Nanometer]]] = None,
  explicitSpatialOffsets:   Option[NonEmptyList[Offset.Q]]                        = None
)

object AdvancedConfig extends AdvancedConfigOptics {

  val Q15: Offset.Q =
    Offset.Q(Angle.arcseconds.reverseGet(15))

  val zeroNm: Quantity[BigDecimal, Nanometer] =
    Quantity[BigDecimal, Nanometer](BigDecimal(0))

  val DefaultYBinning: GmosYBinning =
    GmosYBinning.Two

  val DefaultAmpReadMode: GmosAmpReadMode =
    GmosAmpReadMode.Slow

  val DefaultAmpGain: GmosAmpGain =
    GmosAmpGain.Low

  val DefaultRoi: GmosRoi =
    GmosRoi.FullFrame

  def defaultλDithers[G](
    grating: G
  )(
    implicit calc: DeltaWavelengthCalculator[G]
  ): NonEmptyList[Quantity[BigDecimal, Nanometer]] = {
    val deltaNm = calc.Δλ(grating)
    NonEmptyList.of(zeroNm, deltaNm, deltaNm, zeroNm)
  }

  val DefaultSpatialOffsets: NonEmptyList[Offset.Q] =
    NonEmptyList.of(Offset.Q.Zero, Q15, Q15, Offset.Q.Zero)

  implicit def EqAdvancedConfig[G: Eq, F: Eq, U: Eq]: Eq[AdvancedConfig[G, F, U]] =
    Eq.by { a => (
      a.name,
      a.overrideWavelength,
      a.overrideGrating,
      a.overrideFilter,
      a.overrideFpu,
      a.overrideExposureTimeMode,
      a.explicitXBin,
      a.explicitYBin,
      a.explicitAmpReadMode,
      a.explicitAmpGain,
      a.explicitRoi,
      a.explicitλDithers,
      a.explicitSpatialOffsets
    )}

}

sealed trait AdvancedConfigOptics { self: AdvancedConfig.type =>

  def name[G, F, U]: Lens[AdvancedConfig[G, F, U], Option[NonEmptyString]] =
    Focus[AdvancedConfig[G, F, U]](_.name)

  def overrideWavelength[G, F, U]: Lens[AdvancedConfig[G, F, U], Option[Wavelength]] =
    Focus[AdvancedConfig[G, F, U]](_.overrideWavelength)

  def overrideGrating[G, F, U]: Lens[AdvancedConfig[G, F, U], Option[G]] =
    Focus[AdvancedConfig[G, F, U]](_.overrideGrating)

  def overrideFilter[G, F, U]: Lens[AdvancedConfig[G, F, U], Option[Option[F]]] =
    Focus[AdvancedConfig[G, F, U]](_.overrideFilter)

  def overrideFpu[G, F, U]: Lens[AdvancedConfig[G, F, U], Option[U]] =
    Focus[AdvancedConfig[G, F, U]](_.overrideFpu)

  def overrideExposureMode[G, F, U]: Lens[AdvancedConfig[G, F, U], Option[ExposureTimeMode]] =
    Focus[AdvancedConfig[G, F, U]](_.overrideExposureTimeMode)

  def explicitXBin[G, F, U]: Lens[AdvancedConfig[G, F, U], Option[GmosXBinning]] =
    Focus[AdvancedConfig[G, F, U]](_.explicitXBin)

  def explicitYBin[G, F, U]: Lens[AdvancedConfig[G, F, U], Option[GmosYBinning]] =
    Focus[AdvancedConfig[G, F, U]](_.explicitYBin)

  def explicitAmpReadMode[G, F, U]: Lens[AdvancedConfig[G, F, U], Option[GmosAmpReadMode]] =
    Focus[AdvancedConfig[G, F, U]](_.explicitAmpReadMode)

  def explicitAmpGain[G, F, U]: Lens[AdvancedConfig[G, F, U], Option[GmosAmpGain]] =
    Focus[AdvancedConfig[G, F, U]](_.explicitAmpGain)

  def explicitRoi[G, F, U]: Lens[AdvancedConfig[G, F, U], Option[GmosRoi]] =
    Focus[AdvancedConfig[G, F, U]](_.explicitRoi)

  def explicitλDithers[G, F, U]: Lens[AdvancedConfig[G, F, U], Option[NonEmptyList[Quantity[BigDecimal, Nanometer]]]] =
    Focus[AdvancedConfig[G, F, U]](_.explicitλDithers)

  def explicitSpatialOffsets[G, F, U]: Lens[AdvancedConfig[G, F, U], Option[NonEmptyList[Offset.Q]]] =
    Focus[AdvancedConfig[G, F, U]](_.explicitSpatialOffsets)

}

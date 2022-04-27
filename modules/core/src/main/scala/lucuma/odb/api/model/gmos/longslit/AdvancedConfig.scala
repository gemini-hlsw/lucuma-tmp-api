// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.gmos.longslit

import cats.Eq
import cats.data.NonEmptyList
import coulomb.Quantity
import coulomb.cats.implicits._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.all.{NonEmptyString, PosDouble}
import lucuma.core.`enum`.{GmosAmpGain, GmosAmpReadMode, GmosRoi, GmosXBinning, GmosYBinning, ImageQuality}
import lucuma.core.math.{Angle, Offset}
import lucuma.core.math.units.Nanometer
import lucuma.core.model.SourceProfile
import monocle.{Focus, Lens}

/**
 * AdvancedConfig options provide more control over the sequence that is
 * generated without resorting to a manual sequence.
 */
final case class AdvancedConfig[G, F, U](
  name:                   Option[NonEmptyString],
  overrideBasic:          BasicConfig[G, F, U],
  explicitXBin:           Option[GmosXBinning]                           = None,  // calculated from effective slit and sampling by default
  explicitYBin:           Option[GmosYBinning]                           = None,
  explicitAmpReadMode:    Option[GmosAmpReadMode]                        = None,
  explicitAmpGain:        Option[GmosAmpGain]                            = None,
  explicitRoi:            Option[GmosRoi]                                = None,
  explicitλDithers:       Option[NonEmptyList[Quantity[Int, Nanometer]]] = None,
  explicitSpatialOffsets: Option[NonEmptyList[Offset.Q]]                 = None
) {

  def grating: G =
    overrideBasic.grating

  def filter: Option[F] =
    overrideBasic.filter

  def fpu: U =
    overrideBasic.fpu

  def xBin(
    sourceProfile: SourceProfile,
    imageQuality:  ImageQuality,
    sampling:      PosDouble
  )(implicit calc: XBinCalculator[U]): GmosXBinning =
    explicitXBin.getOrElse(calc.xBin(fpu, sourceProfile, imageQuality, sampling))

  def yBin: GmosYBinning =
    explicitYBin.getOrElse(AdvancedConfig.DefaultYBinning)

  def ampReadMode: GmosAmpReadMode =
    explicitAmpReadMode.getOrElse(AdvancedConfig.DefaultAmpReadMode)

  def ampGain: GmosAmpGain =
    explicitAmpGain.getOrElse(AdvancedConfig.DefaultAmpGain)

  def roi: GmosRoi =
    explicitRoi.getOrElse(AdvancedConfig.DefaultRoi)

  def λDithers(implicit calc: DeltaWavelengthCalculator[G]): NonEmptyList[Quantity[Int, Nanometer]] =
    explicitλDithers.getOrElse(AdvancedConfig.defaultλDithers(grating))

  def spatialOffsets: NonEmptyList[Offset.Q] =
    explicitSpatialOffsets.getOrElse(AdvancedConfig.DefaultSpatialOffsets)

}

object AdvancedConfig extends AdvancedConfigOptics {

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
  ): NonEmptyList[Quantity[Int, Nanometer]] =
      NonEmptyList.of(
        Quantity[Int, Nanometer](0),
        calc.Δλ(grating)
      )

  val DefaultSpatialOffsets: NonEmptyList[Offset.Q] =
    NonEmptyList.of(Offset.Q.Zero, Offset.Q(Angle.arcseconds.reverseGet(15)))

  implicit def EqAdvancedConfig[G: Eq, F: Eq, U: Eq]: Eq[AdvancedConfig[G, F, U]] =
    Eq.by { a => (
      a.name,
      a.overrideBasic,
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

  def overrideBasic[G, F, U]: Lens[AdvancedConfig[G, F, U], BasicConfig[G, F, U]] =
      Focus[AdvancedConfig[G, F, U]](_.overrideBasic)

  def grating[G, F, U]: Lens[AdvancedConfig[G, F, U], G] =
    overrideBasic.andThen(BasicConfig.grating)

  def filter[G, F, U]: Lens[AdvancedConfig[G, F, U], Option[F]] =
    overrideBasic.andThen(BasicConfig.filter)

  def fpu[G, F, U]: Lens[AdvancedConfig[G, F, U], U] =
    overrideBasic.andThen(BasicConfig.fpu)

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

  def explicitλDithers[G, F, U]: Lens[AdvancedConfig[G, F, U], Option[NonEmptyList[Quantity[Int, Nanometer]]]] =
    Focus[AdvancedConfig[G, F, U]](_.explicitλDithers)

  def explicitSpatialOffsets[G, F, U]: Lens[AdvancedConfig[G, F, U], Option[NonEmptyList[Offset.Q]]] =
    Focus[AdvancedConfig[G, F, U]](_.explicitSpatialOffsets)

}
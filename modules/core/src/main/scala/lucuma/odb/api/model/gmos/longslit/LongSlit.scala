// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.gmos.longslit

import cats.data.NonEmptyList
import coulomb.Quantity
import eu.timepit.refined.types.all.PosDouble
import lucuma.core.enums.{GmosAmpGain, GmosAmpReadMode, GmosRoi, GmosXBinning, GmosYBinning, ImageQuality}
import lucuma.core.math.{Offset, Wavelength}
import lucuma.core.math.units.Nanometer
import lucuma.core.model.{ExposureTimeMode, SourceProfile}

trait LongSlit[G, F, U] {

  def basic: BasicConfig[G, F, U]

  def advanced: Option[AdvancedConfig[G, F, U]]

  private def explicitOr[A](explicit: AdvancedConfig[G, F, U] => Option[A], default: => A): A =
    advanced.flatMap(explicit).getOrElse(default)

  def grating: G =
    explicitOr(_.overrideGrating, basic.grating)

  def filter: Option[F] =
    explicitOr(_.overrideFilter, basic.filter)

  def fpu: U =
    explicitOr(_.overrideFpu, basic.fpu)

  def xBin(
    sourceProfile: SourceProfile,
    imageQuality:  ImageQuality,
    sampling:      PosDouble
  )(implicit calc: XBinCalculator[U]): GmosXBinning =
    advanced.flatMap(_.explicitXBin).getOrElse(
     calc.xBin(fpu, sourceProfile, imageQuality, sampling)
    )

  def yBin: GmosYBinning =
    explicitOr(_.explicitYBin, AdvancedConfig.DefaultYBinning)

  def ampReadMode: GmosAmpReadMode =
    explicitOr(_.explicitAmpReadMode, AdvancedConfig.DefaultAmpReadMode)

  def ampGain: GmosAmpGain =
    explicitOr(_.explicitAmpGain, AdvancedConfig.DefaultAmpGain)

  def roi: GmosRoi =
    explicitOr(_.explicitRoi, AdvancedConfig.DefaultRoi)

  def λDithers(implicit calc: DeltaWavelengthCalculator[G]): NonEmptyList[Quantity[BigDecimal, Nanometer]] =
    explicitOr(_.explicitλDithers, AdvancedConfig.defaultλDithers(grating))

  def spatialOffsets: NonEmptyList[Offset.Q] =
    explicitOr(_.explicitSpatialOffsets, AdvancedConfig.DefaultSpatialOffsets)

  def overrideWavelength: Option[Wavelength] =
    advanced.flatMap(_.overrideWavelength)

  def overrideExposureTimeMode: Option[ExposureTimeMode] =
    advanced.flatMap(_.overrideExposureTimeMode)

}

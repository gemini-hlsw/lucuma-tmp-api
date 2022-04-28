// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.gmos.longslit


import cats.Order
import cats.syntax.order._
import coulomb._
import eu.timepit.refined.types.all.{PosDouble, PosInt}
import lucuma.core.`enum`.{GmosNorthDetector, GmosNorthFpu, GmosSouthDetector, GmosSouthFpu, GmosXBinning, ImageQuality, Site}
import lucuma.core.math.Angle
import lucuma.core.math.units.{Nanometer, NanometersPerPixel, Pixels}
import lucuma.core.model.SourceProfile
import spire.math.Rational

private[longslit] sealed trait GmosLongslitMath {

  def site: Site

  def pixelSize: Angle =
    site match {
      case Site.GN => GmosNorthDetector.Hamamatsu.pixelSize
      case Site.GS => GmosSouthDetector.Hamamatsu.pixelSize
    }

  def gapSize: Quantity[PosInt, Pixels] =
    site match {
      case Site.GN => GmosNorthDetector.Hamamatsu.gapSize
      case Site.GS => GmosSouthDetector.Hamamatsu.gapSize
    }

  protected implicit val AngleOrder: Order[Angle] =
    Angle.AngleOrder

  val IfuSlitWidth: Angle =
    Angle.fromMicroarcseconds(310000L)


  private val DescendingXBinning: List[GmosXBinning] =
    GmosXBinning.all.sortBy(b => -b.count)

  /**
   * Calculates the best `GmosXBinning` value to use for longslit observing for
   * the desired sampling.
   *
   * @param slitWidth slit size
   * @param sampling desired sampling rate
   */
  protected def xbin(slitWidth: Angle, sampling: PosDouble): GmosXBinning = {
    val npix  = slitWidth.toMicroarcseconds.toDouble / pixelSize.toMicroarcseconds.toDouble
    DescendingXBinning.find(b => npix / b.count.toDouble >= sampling.value).getOrElse(GmosXBinning.One)
  }

  /**
   * Object angular size estimate based on source profile alone.
   */
  def objectSize(p: SourceProfile): Angle =
    p match {
      case SourceProfile.Point(_)          => Angle.Angle0
      case SourceProfile.Uniform(_)        => Angle.Angle180
      case SourceProfile.Gaussian(fwhm, _) => fwhm
    }

  /**
   * Effective size of a target with the given source profile and image quality.
   */
  def effectiveSize(p: SourceProfile, iq: ImageQuality): Angle =
    objectSize(p) max iq.toAngle

  def effectiveSlitWidth(p: SourceProfile, iq: ImageQuality, slitWidth: Angle): Angle =
    slitWidth min effectiveSize(p, iq)

  /*
   * Calculates the wavelength offsets required to fill in the chip gaps,
   * rounded to the nearest 5 nm.
   *
   * @param dispersion - dispersion in nm/pix (see https://www.gemini.edu/sciops/instruments/gmos/spectroscopy-overview/gratings)
   */
  def Δλ(dispersion: Quantity[Rational, NanometersPerPixel]): Quantity[Int, Nanometer] = {
    val d = dispersion.value.toDouble
    val g = gapSize.value.value
    val v = d * g * 2.0             // raw value, which we round to nearest 5 nm
    ((v/5.0).round * 5.0).toInt.withUnit[Nanometer]
  }

}

private[longslit] object GmosNorthLongslitMath extends GmosLongslitMath {

  override def site: Site = Site.GN

  def effectiveSlitWidth(fpu: GmosNorthFpu, p: SourceProfile, iq: ImageQuality): Angle =
    fpu.slitWidth.getOrElse(IfuSlitWidth) min effectiveSize(p, iq)

  def xbin(fpu: GmosNorthFpu, p: SourceProfile, iq: ImageQuality, sampling: PosDouble): GmosXBinning =
    super.xbin(effectiveSlitWidth(fpu, p, iq), sampling)

}

private[longslit] object GmosSouthLongslitMath extends GmosLongslitMath {

  override def site: Site = Site.GS

  def effectiveSlitWidth(fpu: GmosSouthFpu, p: SourceProfile, iq: ImageQuality): Angle =
    fpu.slitWidth.getOrElse(IfuSlitWidth) min effectiveSize(p, iq)

  def xbin(fpu: GmosSouthFpu, p: SourceProfile, iq: ImageQuality, sampling: PosDouble): GmosXBinning =
    super.xbin(effectiveSlitWidth(fpu, p, iq), sampling)

}

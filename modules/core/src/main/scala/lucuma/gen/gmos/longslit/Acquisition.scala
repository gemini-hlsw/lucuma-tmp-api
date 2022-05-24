// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen.gmos.longslit

import cats.data.NonEmptyList
import cats.syntax.either._
import cats.syntax.option._
import eu.timepit.refined.auto._
import lucuma.core.`enum`.{GmosNorthFilter, GmosNorthFpu, GmosNorthGrating, GmosRoi, GmosSouthFilter, GmosSouthFpu, GmosSouthGrating, GmosXBinning, GmosYBinning}
import lucuma.core.math.Wavelength
import lucuma.core.math.syntax.int._
import lucuma.core.optics.syntax.lens._
import lucuma.core.syntax.time._
import lucuma.gen.gmos.{GmosNorthInitialDynamicConfig, GmosSouthInitialDynamicConfig}
import lucuma.gen.{AcqExposureTime, SequenceState}
import lucuma.odb.api.model.GmosModel.{CustomMask, DynamicOptics, GratingConfig, NorthDynamic, SouthDynamic}
import lucuma.odb.api.model.StepConfig
import lucuma.odb.api.model.syntax.nonnegduration._
import lucuma.odb.api.model.time.NonNegDuration

/**
 * GMOS long slit acquisition steps.
 *
 * @tparam D dynamic config type
 * @tparam G grating type
 * @tparam F filter type
 * @tparam U FPU type
 */
sealed trait Acquisition[D, G, F, U] extends SequenceState[D] {

  def optics: DynamicOptics[D, G, F, U]

  def compute(
    acqFilters:   NonEmptyList[(F, Wavelength)],
    fpu:          U,
    exposureTime: AcqExposureTime,
    λ:            Wavelength,
  ): Acquisition.Steps[D] = {

    def filter: F = acqFilters.toList.minBy { case (_, w) =>
      (λ.toPicometers.value.value - w.toPicometers.value.value).abs
    }._1

    eval {
      for {
        _  <- optics.exposure      := exposureTime
        _  <- optics.filter        := filter.some
        _  <- optics.fpu           := none[Either[CustomMask, U]]
        _  <- optics.gratingConfig := none[GratingConfig[G]]
        _  <- optics.xBin          := GmosXBinning.Two
        _  <- optics.yBin          := GmosYBinning.Two
        _  <- optics.roi           := GmosRoi.Ccd2
        s0 <- scienceStep(0.arcsec, 0.arcsec)

        _  <- optics.exposure      := NonNegDuration.unsafeFrom(20.seconds)
        _  <- optics.fpu           := fpu.asRight.some
        _  <- optics.xBin          := GmosXBinning.One
        _  <- optics.yBin          := GmosYBinning.One
        _  <- optics.roi           := GmosRoi.CentralStamp
        s1 <- scienceStep(10.arcsec, 0.arcsec)

        _  <- optics.exposure      := exposureTime * 4
        s2 <- scienceStep(0.arcsec, 0.arcsec)

      } yield Acquisition.Steps(s0, s1, s2)
    }

  }

}

object Acquisition {

  /**
   * Unique step configurations used to form an acquisition sequence.
   *
   * @param ccd2 image, 2x2 using CCD2 ROI
   * @param p10  20 second exposure, 1x1 Central Stamp, 10 arcsec offset in p
   * @param slit image through the slit
   */
  final case class Steps[D](
    ccd2: StepConfig[D],
    p10:  StepConfig[D],
    slit: StepConfig[D]
  ) {

    val initialAtom: NonEmptyList[StepConfig[D]] =
      NonEmptyList.of(ccd2, p10, slit)

    val repeatingAtom: NonEmptyList[StepConfig[D]] =
      NonEmptyList.of(slit)

  }

  object GmosNorth extends GmosNorthInitialDynamicConfig
                      with Acquisition[NorthDynamic, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] {

    def optics: DynamicOptics[NorthDynamic, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] =
      NorthDynamic
  }

  object GmosSouth extends GmosSouthInitialDynamicConfig
                      with Acquisition[SouthDynamic, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu] {

    def optics: DynamicOptics[SouthDynamic, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu] =
      SouthDynamic
  }

}

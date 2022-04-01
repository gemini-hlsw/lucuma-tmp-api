// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen
package gmos
package longslit

import cats.effect.Sync
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.traverse._
import coulomb.Quantity
import coulomb.refined._
import eu.timepit.refined.types.all.{PosDouble, PosInt}
import fs2.Stream
import lucuma.core.`enum`._
import lucuma.core.math.Wavelength
import lucuma.core.math.syntax.int._
import lucuma.core.math.units._
import lucuma.core.model.SourceProfile
import lucuma.core.optics.syntax.lens._
import lucuma.core.optics.syntax.optional._
import lucuma.gen.syntax.stream._
import lucuma.gen.gmos.longslit.syntax.all._
import lucuma.odb.api.model.GmosModel.{CustomMask, Grating, NorthDynamic, NorthStatic}
import lucuma.odb.api.model.{AtomModel, ScienceConfigurationModel, StepModel}
import spire.std.int._

import scala.concurrent.duration._

/**
 * GMOS North LongSlit observing mode generator.
 */
final case class GmosNorthLongSlit(
  mode:            ScienceConfigurationModel.Modes.GmosNorthLongSlit,
  acqExposureTime: AcqExposureTime,
  sciExposureTime: SciExposureTime,
  λ:               Wavelength,
  sourceProfile:   SourceProfile,
  imageQuality:    ImageQuality,
  sampling:        PosDouble
) {

  val generate: Generator[NorthStatic, NorthDynamic] =

    new Generator[NorthStatic, NorthDynamic] with GmosNorthGenerationSupport {

      def acquisitionSteps[F[_] : Sync]: GmosNorthLongSlit.AcquisitionSteps[F] =
        GmosNorthLongSlit.AcquisitionSteps.generate[F](
          mode.fpu, acqExposureTime, λ
        )

      override val static: NorthStatic =
        NorthStatic(
          detector      = GmosNorthDetector.Hamamatsu,
          mosPreImaging = MosPreImaging.IsNotMosPreImaging,
          nodAndShuffle = Option.empty,
          stageMode     = GmosNorthStageMode.FollowXy
        )

      override def acquisition[F[_] : Sync](
        acquired: F[Boolean]
      ): Stream[F, AtomModel[StepModel[NorthDynamic]]] = {

        val acq = acquisitionSteps[F]

        Stream.eval(atom(acq.ccd2, acq.p10, acq.slit)) ++
          Stream.repeatEval(atom(acq.slit)).evalTakeWhileNot(acquired)
      }

      override def reacquisition[F[_] : Sync](
        acquired: F[Boolean]
      ): Stream[F, AtomModel[StepModel[NorthDynamic]]] = {

        val acq = acquisitionSteps[F]

        Stream.repeatEval(atom(acq.slit)).evalTakeWhileNot(acquired)

      }

      def scienceSteps[F[_] : Sync]: GmosNorthLongSlit.ScienceSteps[F] =
        GmosNorthLongSlit.ScienceSteps.generate[F](
          mode, sciExposureTime, λ, sourceProfile, imageQuality, sampling
        )

      def science[F[_] : Sync](
        observed: F[Boolean]
      ): Stream[F, AtomModel[StepModel[NorthDynamic]]] = {

        val sci = scienceSteps[F]

        Stream.evals(
          List(
            atom(sci.science0, sci.flat0   ),
            atom(sci.flat1,    sci.science1),
            atom(sci.science1, sci.flat1   ),
            atom(sci.flat0,    sci.science0)
          ).sequence
        ).repeat
         .evalTakeWhileNot(observed)
      }

    }

}

/**
 * Sequence generation for GMOS North Longslit
 */
object GmosNorthLongSlit {

  /**
   * Unique step configurations used to form an acquisition sequence.
   *
   * @param ccd2 image, 2x2 using CCD2 ROI
   * @param p10  20 second exposure, 1x1 Central Stamp, 10 arcsec offset in p
   * @param slit image through the slit
   */
  final case class AcquisitionSteps[F[_]](
    ccd2: F[StepModel[NorthDynamic]],
    p10:  F[StepModel[NorthDynamic]],
    slit: F[StepModel[NorthDynamic]]
  )

  object AcquisitionSteps extends GmosNorthGenerationSupport {

    def generate[F[_]: Sync](
      fpu:          GmosNorthFpu,
      exposureTime: AcqExposureTime,
      λ:            Wavelength,
    ): AcquisitionSteps[F] = {

      def filter: GmosNorthFilter = GmosNorthFilter.allAcquisition.minBy { f =>
        (λ.toPicometers.value.value - f.wavelength.toPicometers.value.value).abs
      }

      eval {
        for {
          _  <- NorthDynamic.exposure := exposureTime.value
          _  <- NorthDynamic.filter   := filter.some
          _  <- NorthDynamic.fpu      := none[Either[CustomMask, GmosNorthFpu]]
          _  <- NorthDynamic.grating  := none[Grating[GmosNorthDisperser]]
          _  <- NorthDynamic.xBin     := GmosXBinning.Two
          _  <- NorthDynamic.yBin     := GmosYBinning.Two
          _  <- NorthDynamic.roi      := GmosRoi.Ccd2
          s0 <- scienceStep(0.arcsec, 0.arcsec)

          _  <- NorthDynamic.exposure := 20.seconds
          _  <- NorthDynamic.fpu      := fpu.asRight.some
          _  <- NorthDynamic.xBin     := GmosXBinning.One
          _  <- NorthDynamic.yBin     := GmosYBinning.One
          _  <- NorthDynamic.roi      := GmosRoi.CentralStamp
          s1 <- scienceStep(10.arcsec, 0.arcsec)

          _  <- NorthDynamic.exposure := exposureTime.value * 4
          s2 <- scienceStep(0.arcsec, 0.arcsec)

        } yield AcquisitionSteps(s0, s1, s2)
      }

    }

  }

  /**
   * Unique step configurations used to form a science sequence.
   *
   * @param science0 science step at offset (0, 0) and requested λ
   * @param flat0    smart flat matching `science0`
   * @param science1 science step at offset (0, 15) and λ + Δ
   * @param flat1    smart flat matching `science1`
   */
  final case class ScienceSteps[F[_]](
    science0: F[StepModel[NorthDynamic]],
    flat0:    F[StepModel[NorthDynamic]],
    science1: F[StepModel[NorthDynamic]],
    flat1:    F[StepModel[NorthDynamic]]
  )

  object ScienceSteps extends GmosNorthGenerationSupport {

    def generate[F[_]: Sync](
      mode:          ScienceConfigurationModel.Modes.GmosNorthLongSlit,
      exposureTime:  SciExposureTime,
      λ:             Wavelength,
      sourceProfile: SourceProfile,
      imageQuality:  ImageQuality,
      sampling:      PosDouble
    ): ScienceSteps[F] = {
      def sum(λ: Wavelength, Δ: Quantity[PosInt, Nanometer]): Wavelength =
        new Wavelength(λ.toPicometers + Δ.to[PosInt, Picometer])

      eval {
        for {
          _  <- NorthDynamic.exposure := exposureTime.value
          _  <- NorthDynamic.xBin     := mode.fpu.xbin(sourceProfile, imageQuality, sampling)
          _  <- NorthDynamic.yBin     := GmosYBinning.Two
          _  <- NorthDynamic.grating  := Grating(mode.disperser, GmosDisperserOrder.One, λ).some
          _  <- NorthDynamic.filter   := mode.filter
          _  <- NorthDynamic.fpu      := mode.fpu.asRight.some
          s0 <- scienceStep(0.arcsec, 0.arcsec)
          f0 <- flatStep

          _  <- NorthDynamic.wavelength := sum(λ, mode.disperser.Δλ)
          s1 <- scienceStep(0.arcsec, 15.arcsec)
          f1 <- flatStep
        } yield ScienceSteps(s0, f0, s1, f1)
      }

    }

  }

}

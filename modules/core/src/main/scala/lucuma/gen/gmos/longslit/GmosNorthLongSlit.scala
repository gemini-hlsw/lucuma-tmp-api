// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen
package gmos
package longslit

import cats.effect.Sync
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.option._
import coulomb.Quantity
import coulomb.refined._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{PosDouble, PosInt}
import lucuma.core.`enum`._
import lucuma.core.math.Wavelength
import lucuma.core.math.syntax.int._
import lucuma.core.math.units._
import lucuma.core.model.SourceProfile
import lucuma.core.optics.syntax.lens._
import lucuma.core.optics.syntax.optional._
import lucuma.gen.gmos.longslit.GmosLongSlit.{AcquisitionSteps, ScienceSteps}
import lucuma.gen.gmos.longslit.syntax.all._
import lucuma.itc.client.{ItcClient, ItcResult}
import lucuma.odb.api.model.GmosModel.{CustomMask, GratingConfig, NorthDynamic, NorthStatic}
import lucuma.odb.api.model.{ObservationModel, ScienceConfigurationModel, Sequence}
import lucuma.odb.api.repo.OdbRepo
import spire.std.int._

import scala.concurrent.duration._

sealed trait GmosNorthLongSlit[F[_]] extends GmosNorthGenerator[F]

/**
 * Sequence generation for GMOS North Longslit
 */
object GmosNorthLongSlit {

  /**
   * Queries the ITC and ODB to come up with a GMOS North LongSlit generator,
   * if possible.
   */
  def query[F[_]: Sync](
    itc:         ItcClient[F],
    odb:         OdbRepo[F],
    observation: ObservationModel,
    sampling:    PosDouble = GmosLongSlit.DefaultSampling,
  ): F[Either[ItcResult.Error, Option[GmosNorthLongSlit[F]]]] =

    GmosLongSlit.Input.query(itc, odb, observation, sampling) {
      case gnls: ScienceConfigurationModel.Modes.GmosNorthLongSlit => gnls
    }.map(_.map(_.map(fromInput[F])))

  def fromInput[F[_]: Sync](
    in: GmosLongSlit.Input[ScienceConfigurationModel.Modes.GmosNorthLongSlit]
  ): GmosNorthLongSlit[F] =
    apply(in.mode, in.λ, in.imageQuality, in.sampling, in.sourceProfile, in.acqTime, in.sciTime, in.exposureCount)

  def apply[F[_]: Sync](
    mode:          ScienceConfigurationModel.Modes.GmosNorthLongSlit,
    λ:             Wavelength,
    imageQuality:  ImageQuality,
    sampling:      PosDouble,
    sourceProfile: SourceProfile,
    acqTime:       AcqExposureTime,
    sciTime:       SciExposureTime,
    exposureCount: PosInt
  ): GmosNorthLongSlit[F] =

    new GmosNorthLongSlit[F] with GmosLongSlit[F, NorthStatic, NorthDynamic] {

      override def static: NorthStatic =
        NorthStatic(
          detector      = GmosNorthDetector.Hamamatsu,
          mosPreImaging = MosPreImaging.IsNotMosPreImaging,
          nodAndShuffle = Option.empty,
          stageMode     = GmosNorthStageMode.FollowXy
        )

      override def acquisitionSteps: AcquisitionSteps[NorthDynamic] =
        Acquisition.compute(mode.fpu, acqTime, λ)

      override def scienceSteps: ScienceSteps[NorthDynamic] =
        Science.compute(mode, sciTime, λ, sourceProfile, imageQuality, sampling)

      override def acquisition(
        recordedSteps: List[RecordedStep[NorthDynamic]]
      ): F[Sequence[NorthDynamic]] =
        longSlitAcquisition(recordedSteps)

      override def science(
        recordedSteps: List[RecordedStep[NorthDynamic]]
      ): F[Sequence[NorthDynamic]] =
        longSlitScience(exposureCount, recordedSteps)
    }

  object Acquisition extends GmosNorthSequenceState {

    def compute(
      fpu:          GmosNorthFpu,
      exposureTime: AcqExposureTime,
      λ:            Wavelength,
    ): AcquisitionSteps[NorthDynamic] = {

      def filter: GmosNorthFilter = GmosNorthFilter.allAcquisition.minBy { f =>
        (λ.toPicometers.value.value - f.wavelength.toPicometers.value.value).abs
      }

      eval {
        for {
          _  <- NorthDynamic.exposure      := exposureTime.value
          _  <- NorthDynamic.filter        := filter.some
          _  <- NorthDynamic.fpu           := none[Either[CustomMask, GmosNorthFpu]]
          _  <- NorthDynamic.gratingConfig := none[GratingConfig[GmosNorthDisperser]]
          _  <- NorthDynamic.xBin          := GmosXBinning.Two
          _  <- NorthDynamic.yBin          := GmosYBinning.Two
          _  <- NorthDynamic.roi           := GmosRoi.Ccd2
          s0 <- scienceStep(0.arcsec, 0.arcsec)

          _  <- NorthDynamic.exposure      := 20.seconds
          _  <- NorthDynamic.fpu           := fpu.asRight.some
          _  <- NorthDynamic.xBin          := GmosXBinning.One
          _  <- NorthDynamic.yBin          := GmosYBinning.One
          _  <- NorthDynamic.roi           := GmosRoi.CentralStamp
          s1 <- scienceStep(10.arcsec, 0.arcsec)

          _  <- NorthDynamic.exposure      := exposureTime.value * 4
          s2 <- scienceStep(0.arcsec, 0.arcsec)

        } yield AcquisitionSteps(s0, s1, s2)
      }

    }

  }

  object Science extends GmosNorthSequenceState {

    def compute(
      mode:          ScienceConfigurationModel.Modes.GmosNorthLongSlit,
      exposureTime:  SciExposureTime,
      λ:             Wavelength,
      sourceProfile: SourceProfile,
      imageQuality:  ImageQuality,
      sampling:      PosDouble
    ): ScienceSteps[NorthDynamic] = {

      def sum(λ: Wavelength, Δ: Quantity[PosInt, Nanometer]): Wavelength =
        new Wavelength(λ.toPicometers + Δ.to[PosInt, Picometer])

      eval {
        for {
          _  <- NorthDynamic.exposure      := exposureTime.value
          _  <- NorthDynamic.xBin          := mode.fpu.xbin(sourceProfile, imageQuality, sampling)
          _  <- NorthDynamic.yBin          := GmosYBinning.Two
          _  <- NorthDynamic.gratingConfig := GratingConfig(mode.grating, GmosDisperserOrder.One, λ).some
          _  <- NorthDynamic.filter        := mode.filter
          _  <- NorthDynamic.fpu           := mode.fpu.asRight.some
          s0 <- scienceStep(0.arcsec, 0.arcsec)
          f0 <- flatStep

          _  <- NorthDynamic.wavelength    := sum(λ, mode.grating.Δλ)
          s1 <- scienceStep(0.arcsec, 15.arcsec)
          f1 <- flatStep
        } yield ScienceSteps(s0, f0, s1, f1)
      }

    }

  }

}

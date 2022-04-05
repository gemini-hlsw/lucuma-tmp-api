// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen
package gmos
package longslit

import cats.data.NonEmptyList
import cats.effect.Sync
import cats.syntax.either._
import cats.syntax.eq._
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
import lucuma.gen.gmos.longslit.syntax.all._
import lucuma.itc.client.{ItcClient, ItcResult}
import lucuma.odb.api.model.GmosModel.{CustomMask, Grating, NorthDynamic, NorthStatic}
import lucuma.odb.api.model.{ObservationModel, ScienceConfigurationModel, Sequence, StepConfig}
import lucuma.odb.api.repo.OdbRepo
import spire.std.int._

import scala.concurrent.duration._

sealed trait GmosNorthLongSlit[F[_]] extends GmosNorthGenerator[F]

/**
 * Sequence generation for GMOS North Longslit
 */
object GmosNorthLongSlit {

  def query[F[_]: Sync](
    itc:         ItcClient[F],
    odb:         OdbRepo[F],
    observation: ObservationModel,
    sampling:    PosDouble = 2.5,
  ): F[Either[ItcResult.Error, Option[GmosNorthLongSlit[F]]]] =

    itc
      .query(observation.id, odb)
      .map(_.map { case (target, result) =>
        fromObservationAndItc(observation, sampling, target.sourceProfile, result)
      })

  def fromObservationAndItc[F[_]: Sync](
    observation:   ObservationModel,
    sampling:      PosDouble,
    sourceProfile: SourceProfile,
    itc:           ItcResult.Success
  ): Option[GmosNorthLongSlit[F]] =

    for {
      mode     <- observation.scienceConfiguration.collect {
        case gnls: ScienceConfigurationModel.Modes.GmosNorthLongSlit => gnls
      }
      λ        <- observation.scienceRequirements.spectroscopy.wavelength
      acqTime  <- AcqExposureTime.from(10.seconds)
      sciTime  <- SciExposureTime.from(itc.exposureTime)
      expCount <- PosInt.from(itc.exposures).toOption
    } yield GmosNorthLongSlit[F](
      mode,
      λ,
      observation.constraintSet.imageQuality,
      sampling,
      sourceProfile,
      acqTime,
      sciTime,
      expCount
    )

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

    new GmosNorthLongSlit[F] with GeneratorHelper[NorthDynamic] {

      override def static: NorthStatic =
        NorthStatic(
          detector      = GmosNorthDetector.Hamamatsu,
          mosPreImaging = MosPreImaging.IsNotMosPreImaging,
          nodAndShuffle = Option.empty,
          stageMode     = GmosNorthStageMode.FollowXy
        )

      override def acquisition(
        recordedSteps: List[RecordedStep[NorthDynamic]]
      ): F[Sequence[NorthDynamic]] = {

        val steps = AcquisitionSteps(mode.fpu, acqTime, λ)

        def initialSeq: F[Sequence[NorthDynamic]] = createSequence(steps.initialAtom)
        def repeatSeq:  F[Sequence[NorthDynamic]] = createSequence(steps.repeatingAtom)

        recordedSteps.map(_.stepConfig).lastIndexOfSlice(steps.initialAtom.toList) match {
          case i if i < 0 =>
            // We've never done an acquisition for this observation.
            initialSeq

          case i          =>
            // Starting at the last time we acquired the target, split into
            // the initial sequence and everything that may follow.
            val (in, rm) = recordedSteps.slice(i, Int.MaxValue).splitAt(steps.initialAtom.length)

            def isExecuted: Boolean    = in.forall(_.isExecuted)
            def repeatingSlit: Boolean = rm.map(_.stepConfig).forall(_ === steps.slit)

            // If any of the initial sequence is not yet executed, then we
            // still need to do the initial atom.  Otherwise if we're just
            // repeating the image through the slit, continue to do so.
            if (isExecuted && repeatingSlit) repeatSeq else initialSeq
        }

      }

      override def science(
        recordedSteps: List[RecordedStep[NorthDynamic]]
      ): F[Sequence[NorthDynamic]] = {

        val sciSteps      = ScienceSteps(mode, sciTime, λ, sourceProfile, imageQuality, sampling)
        val wholeSequence = List.unfold(0) { i =>
          Option.when(i < exposureCount)((sciSteps.atom(i), i + 1))
        }

        createSequence(
          remainingSteps(wholeSequence, recordedSteps)(identity)
        )
      }

    }

  /**
   * Unique step configurations used to form an acquisition sequence.
   *
   * @param ccd2 image, 2x2 using CCD2 ROI
   * @param p10  20 second exposure, 1x1 Central Stamp, 10 arcsec offset in p
   * @param slit image through the slit
   */
  final case class AcquisitionSteps(
    ccd2: StepConfig[NorthDynamic],
    p10:  StepConfig[NorthDynamic],
    slit: StepConfig[NorthDynamic]
  ) {

    val initialAtom: NonEmptyList[StepConfig[NorthDynamic]] =
      NonEmptyList.of(ccd2, p10, slit)

    val repeatingAtom: NonEmptyList[StepConfig[NorthDynamic]] =
      NonEmptyList.of(slit)

  }

  object AcquisitionSteps extends GmosNorthSequenceState {

    def apply(
      fpu:          GmosNorthFpu,
      exposureTime: AcqExposureTime,
      λ:            Wavelength,
    ): AcquisitionSteps = {

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
  final case class ScienceSteps(
    science0: StepConfig[NorthDynamic],
    flat0:    StepConfig[NorthDynamic],
    science1: StepConfig[NorthDynamic],
    flat1:    StepConfig[NorthDynamic]
  ) {

    val atom0: NonEmptyList[StepConfig[NorthDynamic]] =
      NonEmptyList.of(science0, flat0)

    val atom1: NonEmptyList[StepConfig[NorthDynamic]] =
      NonEmptyList.of(flat1, science1)

    val atom2: NonEmptyList[StepConfig[NorthDynamic]] =
      NonEmptyList.of(science1, flat1)

    val atom3: NonEmptyList[StepConfig[NorthDynamic]] =
      NonEmptyList.of(flat0, science0)

    val uniqueAtoms: Vector[NonEmptyList[StepConfig[NorthDynamic]]] =
      Vector(atom0, atom1, atom2, atom3)

    def atom(i: Int): NonEmptyList[StepConfig[NorthDynamic]] =
      uniqueAtoms((i % 4).abs)
  }

  object ScienceSteps extends GmosNorthSequenceState {

    def apply(
      mode:          ScienceConfigurationModel.Modes.GmosNorthLongSlit,
      exposureTime:  SciExposureTime,
      λ:             Wavelength,
      sourceProfile: SourceProfile,
      imageQuality:  ImageQuality,
      sampling:      PosDouble
    ): ScienceSteps = {

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

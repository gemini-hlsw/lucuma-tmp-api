// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen
package gmos
package longslit

import cats.Eq
import cats.data.NonEmptyList
import cats.effect.Sync
import cats.syntax.eq._
import cats.syntax.functor._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{PosDouble, PosInt}
import lucuma.core.`enum`.ImageQuality
import lucuma.core.math.Wavelength
import lucuma.core.model.SourceProfile
import lucuma.itc.client.{ItcClient, ItcResult}
import lucuma.odb.api.model.{ObservationModel, ScienceConfigurationModel, Sequence, StepConfig}
import lucuma.odb.api.repo.OdbRepo

import scala.concurrent.duration._

trait GmosLongSlit[F[_], S, D] extends Generator[F, S, D] with GeneratorHelper[D] {

  import GmosLongSlit.{AcquisitionSteps, ScienceSteps}

  def acquisitionSteps: AcquisitionSteps[D]

  def scienceSteps: ScienceSteps[D]

  def longSlitAcquisition(
    recordedSteps: List[RecordedStep[D]]
  )(implicit ev1: Sync[F], ev2: Eq[D]): F[Sequence[D]] = {

    val steps = acquisitionSteps

    def initialSeq: F[Sequence[D]] = createSequence(steps.initialAtom)
    def repeatSeq:  F[Sequence[D]] = createSequence(steps.repeatingAtom)

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

  def longSlitScience(
    exposureCount: Int,
    recordedSteps: List[RecordedStep[D]]
  )(implicit ev1: Sync[F]): F[Sequence[D]] = {

    val sciSteps      = scienceSteps
    val wholeSequence = List.unfold(0) { i =>
      Option.when(i < exposureCount)((sciSteps.atom(i), i + 1))
    }

    createSequence(
      remainingSteps(wholeSequence, recordedSteps)(identity)
    )
  }


}


object GmosLongSlit {

  def acquisitionExposureTime: AcqExposureTime =
    AcqExposureTime.unsafeFrom(10.seconds)

  val DefaultSampling: PosDouble =
    2.5

  final case class Input[M](
    mode:          M,
    λ:             Wavelength,
    imageQuality:  ImageQuality,
    sampling:      PosDouble,
    sourceProfile: SourceProfile,
    acqTime:       AcqExposureTime,
    sciTime:       SciExposureTime,
    exposureCount: PosInt
  )

  object Input {

    def query[F[_]: Sync, M](
      itc:         ItcClient[F],
      odb:         OdbRepo[F],
      observation: ObservationModel,
      sampling:    PosDouble = GmosLongSlit.DefaultSampling,
    )(
      f: PartialFunction[ScienceConfigurationModel, M]
    ): F[Either[ItcResult.Error, Option[Input[M]]]] =

      itc
        .query(observation.id, odb)
        .map(_.map { case (target, result) =>
          fromObservationAndItc(observation, sampling, target.sourceProfile, result)(f)
        })


    def fromObservationAndItc[M](
      observation:   ObservationModel,
      sampling:      PosDouble,
      sourceProfile: SourceProfile,
      itc:           ItcResult.Success
    )(
      f: PartialFunction[ScienceConfigurationModel, M]
    ): Option[Input[M]] =

      for {
        mode     <- observation.scienceConfiguration.collect(f)
        λ        <- observation.scienceRequirements.spectroscopy.wavelength
        sciTime  <- SciExposureTime.from(itc.exposureTime)
        expCount <- PosInt.from(itc.exposures).toOption
      } yield Input[M](
        mode,
        λ,
        observation.constraintSet.imageQuality,
        sampling,
        sourceProfile,
        GmosLongSlit.acquisitionExposureTime,
        sciTime,
        expCount
      )

  }

  /**
   * Unique step configurations used to form an acquisition sequence.
   *
   * @param ccd2 image, 2x2 using CCD2 ROI
   * @param p10  20 second exposure, 1x1 Central Stamp, 10 arcsec offset in p
   * @param slit image through the slit
   */
  final case class AcquisitionSteps[D](
    ccd2: StepConfig[D],
    p10:  StepConfig[D],
    slit: StepConfig[D]
  ) {

    val initialAtom: NonEmptyList[StepConfig[D]] =
      NonEmptyList.of(ccd2, p10, slit)

    val repeatingAtom: NonEmptyList[StepConfig[D]] =
      NonEmptyList.of(slit)

  }

  /**
   * Unique step configurations used to form a science sequence.
   *
   * @param science0 science step at offset (0, 0) and requested λ
   * @param flat0    smart flat matching `science0`
   * @param science1 science step at offset (0, 15) and λ + Δ
   * @param flat1    smart flat matching `science1`
   */
  final case class ScienceSteps[D](
    science0: StepConfig[D],
    flat0:    StepConfig[D],
    science1: StepConfig[D],
    flat1:    StepConfig[D]
  ) {

    val atom0: NonEmptyList[StepConfig[D]] =
      NonEmptyList.of(science0, flat0)

    val atom1: NonEmptyList[StepConfig[D]] =
      NonEmptyList.of(flat1, science1)

    val atom2: NonEmptyList[StepConfig[D]] =
      NonEmptyList.of(science1, flat1)

    val atom3: NonEmptyList[StepConfig[D]] =
      NonEmptyList.of(flat0, science0)

    val uniqueAtoms: Vector[NonEmptyList[StepConfig[D]]] =
      Vector(atom0, atom1, atom2, atom3)

    def atom(i: Int): NonEmptyList[StepConfig[D]] =
      uniqueAtoms((i % 4).abs)

  }
}
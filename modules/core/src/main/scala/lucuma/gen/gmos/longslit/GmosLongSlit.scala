// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen
package gmos
package longslit

import cats.Eq
import cats.effect.Sync
import cats.syntax.eq._
import cats.syntax.functor._
import coulomb.Quantity
import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{PosDouble, PosInt}
import lucuma.core.`enum`.ImageQuality
import lucuma.core.math.Wavelength
import lucuma.core.math.units.Nanometer
import lucuma.core.model.SourceProfile
import lucuma.core.syntax.time._
import lucuma.itc.client.{ItcClient, ItcResult}
import lucuma.odb.api.model.{ObservationModel, ScienceMode, Sequence}
import lucuma.odb.api.repo.OdbRepo

trait GmosLongSlit[F[_], S, D] extends Generator[F, S, D] with GeneratorHelper[D] {

  def λ: Wavelength

  def acquisitionSteps: Acquisition.Steps[D]

  def scienceAtoms: LazyList[Science.Atom[D]]

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

        def isExecuted: Boolean    = in.forall(_.successfullyExecuted)
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

    val wholeSequence = scienceAtoms.take(exposureCount).toList.map(_.steps)

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

  def wavelengthDither(λ: Wavelength, Δ: Quantity[Int, Nanometer]): Wavelength =
    Wavelength
      .fromPicometers
      .getOption(λ.toPicometers.value + Δ.value * 1000)
      .getOrElse(Wavelength.Min)


  final case class Input[M](
    mode:          M,
    requirementλ:  Wavelength,
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
      f: PartialFunction[ScienceMode, M]
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
      f: PartialFunction[ScienceMode, M]
    ): Option[Input[M]] =

      for {
        mode     <- observation.scienceMode.collect(f)
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

}

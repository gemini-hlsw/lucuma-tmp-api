// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen
package gmos
package longslit

import cats.{Eq, Order}
import cats.data.EitherT
import cats.Order.catsKernelOrderingForOrder
import cats.effect.Sync
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.functor._
import cats.syntax.traverse._
import coulomb.Quantity
import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{NonNegInt, PosDouble}
import lucuma.core.`enum`.ImageQuality
import lucuma.core.math.{Angle, Wavelength}
import lucuma.core.math.units.Nanometer
import lucuma.core.model.{NonNegDuration, SourceProfile, Target}
import lucuma.core.syntax.time._
import lucuma.itc.client.ItcClient
import lucuma.odb.api.model.{ExposureTimeMode, ObservationModel, ScienceMode, Sequence}
import lucuma.odb.api.model.ExposureTimeMode.{FixedExposure, SignalToNoise}
import lucuma.odb.api.model.gmos.longslit.{GmosLongslitMath, LongSlit}
import lucuma.odb.api.repo.OdbRepo

trait GmosLongSlit[F[_], S, D] extends Generator[F, S, D] with GeneratorHelper[D] {

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
    λ:             Wavelength,
    imageQuality:  ImageQuality,
    sampling:      PosDouble,
    sourceProfile: SourceProfile,
    acqTime:       AcqExposureTime,
    sciTime:       SciExposureTime,
    exposureCount: NonNegInt
  )

  object Input {

    def query[F[_] : Sync, M <: LongSlit[_, _, _]](
      itc:            ItcClient[F],
      odb:            OdbRepo[F],
      observation:    ObservationModel,
      sampling:       PosDouble = GmosLongSlit.DefaultSampling,
      includeDeleted: Boolean = false,
      useCache:       Boolean = true
    )(
      f: PartialFunction[ScienceMode, M]
    ): F[Either[String, Input[M]]] = {

      // Either the explicit override wavelength or else fall back on the
      // science requirements wavelength
      def requestedWavelength(mode: M): Option[Wavelength] =
        mode.advanced
          .flatMap(_.overrideWavelength)
          .orElse(observation.scienceRequirements.spectroscopy.wavelength)

      // Either the explicit override exposure time mode, or else fall back om
      // the signal to noise exposure time mode in the science requirements.
      def requestedExposureTimeMode(mode: M): Option[ExposureTimeMode] =
          mode
            .advanced
            .flatMap(_.overrideExposureTimeMode)
            .orElse(
              observation.scienceRequirements.spectroscopy.signalToNoise.map(sn => SignalToNoise(sn))
            )

      val targets: F[List[Target]] =
        observation
          .targetEnvironment
          .asterism
          .toList
          .flatTraverse { tid =>
            odb.target.selectTarget(tid, includeDeleted).map(_.map(_.target).toList)
          }

      // Select the source profile that yields the largest object size
      implicit val AngleOrder: Order[Angle] =
        Angle.AngleOrder

      val sourceProfile: F[Option[SourceProfile]] =
        targets.map { ts =>
          ts.maxByOption { t =>
            GmosLongslitMath.objectSize(t.sourceProfile)
          }.map(_.sourceProfile)
        }

      val queryItc: F[Either[String, FixedExposure]] =
        itc
          .query(observation.id, odb, includeDeleted, useCache)
          .map {
            _.leftMap(e => s"Problem querying the ITC: ${e.msg}")
             .map { case (_, s) => FixedExposure(s.exposures, s.exposureTime) }
          }

      (for {
        mode <- EitherT.fromOption[F](
          observation.scienceMode.collect(f),
          "There is no matching GMOS Long Slit mode definition"
        )

        λ <- EitherT.fromOption[F](
          requestedWavelength(mode),
          "Could not determine the wavelength"
        )

        sp <- EitherT(sourceProfile.map(_.toRight("Observation has no targets")))

        sci0 <- EitherT.fromOption[F](
          requestedExposureTimeMode(mode),
          "Observation S/N requirement not specified"
        )

        sci <- sci0 match {
          case SignalToNoise(_)      => EitherT(queryItc)
          case f@FixedExposure(_, _) => EitherT.pure[F, String](f)
        }

      } yield Input[M](
        mode,
        λ,
        observation.constraintSet.imageQuality,
        sampling,
        sp,
        GmosLongSlit.acquisitionExposureTime,
        shapeless.tag[SciExposureTimeTag][NonNegDuration](sci.time),
        sci.count
      )).value
    }

  }
}

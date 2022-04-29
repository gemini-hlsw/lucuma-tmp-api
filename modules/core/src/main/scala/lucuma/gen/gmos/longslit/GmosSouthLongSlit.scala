// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen
package gmos
package longslit

import cats.effect.Sync
import cats.syntax.functor._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{PosDouble, PosInt}
import lucuma.core.`enum`._
import lucuma.core.math.Wavelength
import lucuma.core.model.SourceProfile
import lucuma.itc.client.{ItcClient, ItcResult}
import lucuma.odb.api.model.GmosModel.{SouthDynamic, SouthStatic}
import lucuma.odb.api.model.{ObservationModel, ScienceMode, Sequence}
import lucuma.odb.api.model.gmos.syntax.gmosSouthFilter._
import lucuma.odb.api.repo.OdbRepo

sealed trait GmosSouthLongSlit[F[_]] extends GmosSouthGenerator[F]


/**
 * Sequence generation for GMOS South Longslit
 */
object GmosSouthLongSlit {

  /**
   * Queries the ITC and ODB to come up with a GMOS South LongSlit generator,
   * if possible.
   */
  def query[F[_]: Sync](
    itc:         ItcClient[F],
    odb:         OdbRepo[F],
    observation: ObservationModel,
    sampling:    PosDouble = GmosLongSlit.DefaultSampling,
  ): F[Either[ItcResult.Error, Option[GmosSouthLongSlit[F]]]] =

    GmosLongSlit.Input.query(itc, odb, observation, sampling) {
      case gnls: ScienceMode.GmosSouthLongSlit => gnls
    }.map(_.map(_.map(fromInput[F])))

  def fromInput[F[_]: Sync](
    in: GmosLongSlit.Input[ScienceMode.GmosSouthLongSlit]
  ): GmosSouthLongSlit[F] =
    apply(in.mode, in.λ, in.imageQuality, in.sampling, in.sourceProfile, in.acqTime, in.sciTime, in.exposureCount)

  def apply[F[_]: Sync](
    mode:          ScienceMode.GmosSouthLongSlit,
    λ:             Wavelength,
    imageQuality:  ImageQuality,
    sampling:      PosDouble,
    sourceProfile: SourceProfile,
    acqTime:       AcqExposureTime,
    sciTime:       SciExposureTime,
    exposureCount: PosInt
  ): GmosSouthLongSlit[F] =

    new GmosSouthLongSlit[F] with GmosLongSlit[F, SouthStatic, SouthDynamic] {

      override def static: SouthStatic =
        SouthStatic(
          detector      = GmosSouthDetector.Hamamatsu,
          mosPreImaging = MosPreImaging.IsNotMosPreImaging,
          nodAndShuffle = Option.empty,
          stageMode     = GmosSouthStageMode.FollowXy
        )

      override def acquisitionSteps: Acquisition.Steps[SouthDynamic] =
        Acquisition.GmosSouth.compute(
          GmosSouthFilter.allAcquisition.fproduct(_.wavelength),
          mode.fpu, acqTime, λ
        )

      override def scienceAtoms: LazyList[Science.Atom[SouthDynamic]] =
        Science.GmosSouth.compute(mode, sciTime, λ, sourceProfile, imageQuality, sampling)

      override def acquisition(
        recordedSteps: List[RecordedStep[SouthDynamic]]
      ): F[Sequence[SouthDynamic]] =
        longSlitAcquisition(recordedSteps)

      override def science(
        recordedSteps: List[RecordedStep[SouthDynamic]]
      ): F[Sequence[SouthDynamic]] =
        longSlitScience(exposureCount, recordedSteps)
    }

}

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen
package gmos
package longslit

import cats.effect.Sync
import cats.syntax.functor._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.PosDouble
import lucuma.core.`enum`._
import lucuma.itc.client.ItcClient
import lucuma.odb.api.model.{ObservationModel, ScienceMode, Sequence}
import lucuma.odb.api.model.gmos.syntax.gmosNorthFilter._
import lucuma.odb.api.model.GmosModel.{NorthDynamic, NorthStatic}
import lucuma.odb.api.repo.OdbRepo

import scala.collection.immutable.LazyList

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
  ): F[Either[String, GmosNorthLongSlit[F]]] =

    GmosLongSlit.Input.query(itc, odb, observation, sampling) {
      case gnls: ScienceMode.GmosNorthLongSlit => gnls
    }.map(_.map(fromInput[F]))

  def fromInput[F[_]: Sync](
    in: GmosLongSlit.Input[ScienceMode.GmosNorthLongSlit]
  ): GmosNorthLongSlit[F] =

    new GmosNorthLongSlit[F] with GmosLongSlit[F, NorthStatic, NorthDynamic] {

      override def static: NorthStatic =
        NorthStatic(
          detector      = GmosNorthDetector.Hamamatsu,
          mosPreImaging = MosPreImaging.IsNotMosPreImaging,
          nodAndShuffle = Option.empty,
          stageMode     = GmosNorthStageMode.FollowXy
        )

      override def acquisitionSteps: Acquisition.Steps[NorthDynamic] =
        Acquisition.GmosNorth.compute(
          GmosNorthFilter.allAcquisition.fproduct(_.wavelength),
          in.mode.fpu, in.acqTime, in.λ
        )

      override def scienceAtoms: LazyList[Science.Atom[NorthDynamic]] =
        Science.GmosNorth.compute(in.mode, in.sciTime, in.λ, in.sourceProfile, in.imageQuality, in.sampling)

      override def acquisition(
        recordedSteps: List[RecordedStep[NorthDynamic]]
      ): F[Sequence[NorthDynamic]] =
        longSlitAcquisition(recordedSteps)

      override def science(
        recordedSteps: List[RecordedStep[NorthDynamic]]
      ): F[Sequence[NorthDynamic]] =
        longSlitScience(in.exposureCount, recordedSteps)
    }

}

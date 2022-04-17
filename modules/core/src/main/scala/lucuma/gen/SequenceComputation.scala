// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen

import cats.{Applicative, Monad}
import cats.effect.Sync
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.traverse._
import lucuma.core.model.Observation
import lucuma.gen.gmos.longslit.GmosNorthLongSlit
import lucuma.gen.gmos.{GmosNorthGenerator, GmosSouthGenerator}
import lucuma.itc.client.{ItcClient, ItcResult}
import lucuma.odb.api.model.{ExecutionContext, ExecutionModel, GmosModel, ObservationModel, ScienceConfigurationModel, Visit, VisitRecord, VisitRecords}
import lucuma.odb.api.repo.OdbRepo

/**
 * Utilities for producing an execution config for an observation, preferring
 * the manually defined sequences (if any) over automated sequences.
 */
object SequenceComputation {

  def manual[F[_]: Sync](
    oid: Observation.Id,
    odb: OdbRepo[F]
  ): F[Option[ExecutionContext]] = {

    def run(o: ObservationModel): F[Option[ExecutionContext]] =
      o.config.flatMap {
        case gn: ExecutionModel.GmosNorth => Instrument.GmosNorth.run(oid, odb, GmosNorthGenerator.manual(gn)).some
        case gs: ExecutionModel.GmosSouth => Instrument.GmosSouth.run(oid, odb, GmosSouthGenerator.manual(gs)).some
        case _                            => None
      }.sequence

    for {
      o <- odb.observation.select(oid)
      e <- o.flatTraverse(run)
    } yield e
  }

  def auto[F[_]: Sync](
    oid: Observation.Id,
    itc: ItcClient[F],
    odb: OdbRepo[F]
  ): F[Option[ExecutionContext]] = {

    def run[S, D, G <: Generator[F, S, D]](
      inst: Instrument.Config[S, D],
      queryResult: F[Either[ItcResult.Error, Option[G]]]
    ): F[Option[ExecutionContext]] =
      queryResult
        .flatMap(_.leftMap(e => new Exception(e.msg)).liftTo[F])
        .flatMap(_.traverse(inst.run(oid, odb, _)))

    def go(o: ObservationModel): F[Option[ExecutionContext]] =
      o.scienceConfiguration.flatTraverse {
        case _: ScienceConfigurationModel.Modes.GmosNorthLongSlit =>
          run(Instrument.GmosNorth, GmosNorthLongSlit.query(itc, odb, o))
        case _                                                    =>
          Applicative[F].pure(None)
      }

    for {
      o <- odb.observation.select(oid)
      e <- o.flatTraverse(go)
    } yield e

  }

  def compute[F[_]: Sync](
    oid: Observation.Id,
    itc: ItcClient[F],
    odb: OdbRepo[F]
  ): F[Option[ExecutionContext]] =

    for {
      m <- manual(oid, odb)
      r <- m.fold(auto(oid, itc, odb)) { m => Applicative[F].pure(m.some) }
    } yield r


  private object Instrument {

    final class Config[S, D](
      visits: VisitRecords => List[(Visit.Id, VisitRecord[S, D])],
      ctx: Observation.Id => ExecutionModel.Config[S, D] => ExecutionContext
    ) {

      def run[F[_] : Monad](
        oid: Observation.Id,
        odb: OdbRepo[F],
        gen: Generator[F, S, D]
      ): F[ExecutionContext] =
        RecordedStep
          .lookup[F, S, D](odb, oid, visits)
          .flatMap(gen.run)
          .map(ctx(oid))
    }

    val GmosNorth: Config[GmosModel.NorthStatic, GmosModel.NorthDynamic] =
      new Config(
        VisitRecords.listGmosNorthVisits,
        oid => execConfig => ExecutionContext.GmosNorth(oid, ExecutionModel.GmosNorth(execConfig))
      )

    val GmosSouth: Config[GmosModel.SouthStatic, GmosModel.SouthDynamic] =
      new Config(
        VisitRecords.listGmosSouthVisits,
        oid => execConfig => ExecutionContext.GmosSouth(oid, ExecutionModel.GmosSouth(execConfig))
      )
  }


}

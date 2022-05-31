// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen

import cats.data.EitherT
import cats.{Applicative, Monad}
import cats.effect.Sync
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.traverse._
import lucuma.core.model.Observation
import lucuma.gen.gmos.longslit.{GmosNorthLongSlit, GmosSouthLongSlit}
import lucuma.gen.gmos.{GmosNorthGenerator, GmosSouthGenerator}
import lucuma.itc.client.ItcClient
import lucuma.odb.api.model.{ExecutionContext, ExecutionModel, GmosModel, ObservationModel, ScienceMode, Visit, VisitRecord, VisitRecords}
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
      o.manualConfig.flatMap {
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
  ): F[Either[String, ExecutionContext]] =
    (for {
      o  <- EitherT.fromOptionF(odb.observation.select(oid), s"Could not find observation `${oid.toString}``")
      m  <- EitherT.fromOption(o.scienceMode, "The science mode has not been selected")
      ec <- EitherT(m match {
            case _: ScienceMode.GmosNorthLongSlit =>
              GmosNorthLongSlit.query(itc, odb, o).flatMap { g =>
                g.traverse(Instrument.GmosNorth.run(oid, odb, _))
              }

            case _: ScienceMode.GmosSouthLongSlit =>
              GmosSouthLongSlit.query(itc, odb, o).flatMap { g =>
                g.traverse(Instrument.GmosSouth.run(oid, odb, _))
              }

            case _                                =>
              Applicative[F].pure(s"Cannot calculate the sequence for ${m.mode.name}".asLeft[ExecutionContext])
           })
    } yield ec).value


  def compute[F[_]: Sync](
    oid: Observation.Id,
    itc: ItcClient[F],
    odb: OdbRepo[F]
  ): F[Either[String, ExecutionContext]] =

    for {
      m <- manual(oid, odb)
      r <- m.fold(auto(oid, itc, odb)) { m => Applicative[F].pure(m.asRight[String]) }
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

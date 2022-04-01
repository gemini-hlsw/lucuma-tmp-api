// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.effect.Async
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import lucuma.itc.client.ItcResult
import lucuma.odb.api.model.{InstrumentConfigModel, ObservationModel}
import lucuma.odb.api.model.targetModel.TargetModel


sealed trait SequenceGenerator[F[_]] {

  def itc(
    observation:    ObservationModel,
    includeDeleted: Boolean = false
  ): F[Option[(TargetModel, ItcResult.Success)]]

  def gen(
    observation:    ObservationModel,
    includeDeleted: Boolean = false
  ): F[Option[InstrumentConfigModel]]

}

object SequenceGenerator {

  def apply[F[_]: Async](
    ctx:         OdbCtx[F],
    useItcCache: Boolean = true
  ): SequenceGenerator[F] =

    new SequenceGenerator[F] {

      override def itc(
        observation:    ObservationModel,
        includeDeleted: Boolean
      ): F[Option[(TargetModel, ItcResult.Success)]] = {
        val tids = observation.targetEnvironment.asterism.toList

        for {
          ts <- tids.flatTraverse(tid => ctx.odbRepo.target.selectTarget(tid, includeDeleted).map(_.toList))
          rs <- ts.traverse(t => ctx.itcClient.query(observation, t.target, useItcCache))
          r  <- ts.zip(rs)
                  .flatMap(_.sequence.toList)
                  .traverse(_.traverse(_.itc.toEither))
                  .map(_.maxByOption { case (_, r) => r.exposureTime })
                  .leftMap(e => new Exception(e.msg))
                  .liftTo[F]
        } yield r
      }

      override def gen(
        observation:    ObservationModel,
        includeDeleted: Boolean
      ): F[Option[InstrumentConfigModel]] = {
        // TBD
        ???
      }


    }

}

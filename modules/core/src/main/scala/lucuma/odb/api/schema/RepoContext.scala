// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{AsterismModel, ObservationModel, ProgramModel, TargetModel}
import lucuma.odb.api.repo.{AsterismRepo, ObservationRepo, OdbRepo, ProgramRepo, TargetRepo}
import cats.effect.Effect
import cats.effect.implicits._
import sangria.schema.Context

import scala.concurrent.Future

final class RepoContextOps[F[_]: Effect, A](val self: Context[OdbRepo[F], A]) {

  def asterismId: AsterismModel.Id =
    self.arg(AsterismSchema.AsterismIdArgument)

  def observationId: ObservationModel.Id =
    self.arg(ObservationSchema.ObservationIdArgument)

  def programId: ProgramModel.Id =
    self.arg(ProgramSchema.ProgramIdArgument)

  def targetId: TargetModel.Id =
    self.arg(TargetSchema.TargetIdArgument)

  def includeDeleted: Boolean =
    self.arg(GeneralSchema.ArgumentIncludeDeleted)


  def asterism[B](f: AsterismRepo[F] => F[B]): Future[B] =
    f(self.ctx.asterism).toIO.unsafeToFuture()

  def observation[B](f: ObservationRepo[F] => F[B]): Future[B] =
    f(self.ctx.observation).toIO.unsafeToFuture()

  def program[B](f: ProgramRepo[F] => F[B]): Future[B] =
    f(self.ctx.program).toIO.unsafeToFuture()

  def target[B](f: TargetRepo[F] => F[B]): Future[B] =
    f(self.ctx.target).toIO.unsafeToFuture()

}

trait ToRepoContextOps {
  implicit def toRepoContextOps[F[_]: Effect, A](self: Context[OdbRepo[F], A]): RepoContextOps[F, A] =
    new RepoContextOps[F, A](self)
}

object context extends ToRepoContextOps

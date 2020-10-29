// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.repo.{AsterismRepo, ObservationRepo, OdbRepo, ProgramRepo, TargetRepo}
import lucuma.core.model.{Asterism, Observation, Program, Target}
import cats.effect.Effect
import cats.effect.implicits._
import sangria.schema.Context

import scala.concurrent.Future

final class RepoContextOps[F[_]: Effect, A](val self: Context[OdbRepo[F], A]) {

  def asterismId: Asterism.Id =
    self.arg(AsterismSchema.AsterismIdArgument)

  def optionalAsterismId: Option[Asterism.Id] =
    self.arg(AsterismSchema.OptionalAsterismIdArgument)

  def observationId: Observation.Id =
    self.arg(ObservationSchema.ObservationIdArgument)

  def optionalObservationId: Option[Observation.Id] =
    self.arg(ObservationSchema.OptionalObservationIdArgument)

  def programId: Program.Id =
    self.arg(ProgramSchema.ProgramIdArgument)

  def optionalProgramId: Option[Program.Id] =
    self.arg(ProgramSchema.OptionalProgramIdArgument)

  def targetId: Target.Id =
    self.arg(TargetSchema.TargetIdArgument)

  def optionalTargetId: Option[Target.Id] =
    self.arg(TargetSchema.OptionalTargetIdArgument)

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

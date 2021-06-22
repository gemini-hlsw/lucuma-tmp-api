// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.repo.{AsterismRepo, ExecutionEventRepo, ObservationRepo, OdbRepo, ProgramRepo, StepRepo, TargetRepo}
import lucuma.core.model.{Asterism, Observation, Program, Target}
import cats.effect.Effect
import cats.effect.implicits._
import cats.syntax.all._
import lucuma.core.util.Gid
import lucuma.odb.api.model.{ExecutionEvent, InputError}
import sangria.schema.Context

import scala.concurrent.Future

final class RepoContextOps[F[_]: Effect](val self: Context[OdbRepo[F], _]) {

  def asterismId: Asterism.Id =
    self.arg(AsterismSchema.AsterismIdArgument)

  def optionalAsterismId: Option[Asterism.Id] =
    self.arg(AsterismSchema.OptionalAsterismIdArgument)

  def executionEventId: ExecutionEvent.Id =
    self.arg(ExecutionEventSchema.ExecutionEventArgument)

  def optionalExecutionEventId: Option[ExecutionEvent.Id] =
    self.arg(ExecutionEventSchema.OptionalExecutionEventArgument)

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

  def pagingFirst: Option[Int] =
    self.arg(Paging.ArgumentPagingFirst)

  def pagingCursor[A](msg: String)(f: Paging.Cursor => Option[A]): Either[InputError, Option[A]] = {

    type E[X] = Either[InputError, X]

    self.arg(Paging.ArgumentPagingCursor).traverse { cursor =>
      f(cursor).toRight(
        InputError.fromMessage(s"Unexpected cursor format ${cursor.toString} (${cursor.toBase64}): $msg")
      ): E[A]
    }
  }


  /** Treats the cursor as a Gid, decoding through Cursor to its Gid representation. */
  def pagingGid[A: Gid](name: String): Either[InputError, Option[A]] =
    pagingCursor(s"Cannot read as $name")(Paging.Cursor.gid[A].getOption)

  def pagingAsterismId: Either[InputError, Option[Asterism.Id]] =
    pagingGid[Asterism.Id]("AsterismId")

  def pagingExecutionEventId: Either[InputError, Option[ExecutionEvent.Id]] =
    pagingGid[ExecutionEvent.Id]("ExecutionEventId")

  def pagingObservationId: Either[InputError, Option[Observation.Id]] =
    pagingGid[Observation.Id]("ObservationId")

  def pagingProgramId: Either[InputError, Option[Program.Id]] =
    pagingGid[Program.Id]("ProgramId")

  def pagingTargetId: Either[InputError, Option[Target.Id]] =
    pagingGid[Target.Id]("TargetId")

  def asterism[B](f: AsterismRepo[F] => F[B]): Future[B] =
    f(self.ctx.asterism).toIO.unsafeToFuture()

  def executionEvent[B](f: ExecutionEventRepo[F] => F[B]): Future[B] =
    f(self.ctx.executionEvent).toIO.unsafeToFuture()

  def observation[B](f: ObservationRepo[F] => F[B]): Future[B] =
    f(self.ctx.observation).toIO.unsafeToFuture()

  def program[B](f: ProgramRepo[F] => F[B]): Future[B] =
    f(self.ctx.program).toIO.unsafeToFuture()

  def step[B](f: StepRepo[F] => F[B]): Future[B] =
    f(self.ctx.step).toIO.unsafeToFuture()

  def target[B](f: TargetRepo[F] => F[B]): Future[B] =
    f(self.ctx.target).toIO.unsafeToFuture()

}

trait ToRepoContextOps {
  implicit def toRepoContextOps[F[_]: Effect](self: Context[OdbRepo[F], _]): RepoContextOps[F] =
    new RepoContextOps[F](self)
}

object context extends ToRepoContextOps

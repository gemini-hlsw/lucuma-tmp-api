// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.model.{ExecutionEvent, Observation, Program, Step, Target}
import lucuma.odb.api.repo.{AtomRepo, ExecutionEventRepo, ObservationRepo, ProgramRepo, StepRepo, TargetRepo}

import cats.effect.std.Dispatcher
import cats.syntax.all._
import lucuma.core.util.Gid
import lucuma.odb.api.model.InputError
import sangria.schema.Context

import scala.concurrent.Future

final class OdbContextOps[F[_]](val self: Context[OdbCtx[F], _]) {

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
    self.arg(TargetSchema.ArgumentTargetId)

  def optionalTargetId: Option[Target.Id] =
    self.arg(TargetSchema.ArgumentOptionalTargetId)

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

  def pagingExecutionEventId: Either[InputError, Option[ExecutionEvent.Id]] =
    pagingGid[ExecutionEvent.Id]("ExecutionEventId")

  def pagingObservationId: Either[InputError, Option[Observation.Id]] =
    pagingGid[Observation.Id]("ObservationId")

  def pagingProgramId: Either[InputError, Option[Program.Id]] =
    pagingGid[Program.Id]("ProgramId")

  def pagingStepId: Either[InputError, Option[Step.Id]] =
    pagingGid[Step.Id]("StepId")

  def pagingTargetId: Either[InputError, Option[Target.Id]] =
    pagingGid[Target.Id]("TargetId")

  def unsafeToFuture[B](fb: F[B])(implicit ev: Dispatcher[F]): Future[B] =
    implicitly[Dispatcher[F]].unsafeToFuture(fb)

  def atom[B](f: AtomRepo[F] => F[B])(implicit ev: Dispatcher[F]): Future[B] =
    unsafeToFuture(f(self.ctx.odbRepo.atom))

  def executionEvent[B](f: ExecutionEventRepo[F] => F[B])(implicit ev: Dispatcher[F]): Future[B] =
    unsafeToFuture(f(self.ctx.odbRepo.executionEvent))

  def observation[B](f: ObservationRepo[F] => F[B])(implicit ev: Dispatcher[F]): Future[B] =
    unsafeToFuture(f(self.ctx.odbRepo.observation))

  def program[B](f: ProgramRepo[F] => F[B])(implicit ev: Dispatcher[F]): Future[B] =
    unsafeToFuture(f(self.ctx.odbRepo.program))

  def step[B](f: StepRepo[F] => F[B])(implicit ev: Dispatcher[F]): Future[B] =
    unsafeToFuture(f(self.ctx.odbRepo.step))

  def target[B](f: TargetRepo[F] => F[B])(implicit ev: Dispatcher[F]): Future[B] =
    unsafeToFuture(f(self.ctx.odbRepo.target))

}

trait ToRepoContextOps {
  implicit def toRepoContextOps[F[_]](self: Context[OdbCtx[F], _]): OdbContextOps[F] =
    new OdbContextOps[F](self)
}

object context extends ToRepoContextOps

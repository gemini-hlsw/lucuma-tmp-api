// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.model.{ExecutionEvent, Observation, Program, Target}
import lucuma.odb.api.repo.{DatasetRepo, ExecutionEventRepo, ObservationRepo, OdbCtx, ProgramRepo, TargetRepo}
import cats.effect.std.Dispatcher
import cats.syntax.all._
import eu.timepit.refined.types.all.NonNegInt
import lucuma.odb.api.model.{Atom, Step, Visit}
import sangria.schema.Context

import scala.concurrent.Future

final class OdbContextOps[F[_]](val self: Context[OdbCtx[F], _]) {

  def atomId: Atom.Id =
    self.arg(AtomSchema.ArgumentAtomId)

  def executionEventId: ExecutionEvent.Id =
    self.arg(ExecutionEventSchema.ExecutionEventArgument)

  def optionalExecutionEventId: Option[ExecutionEvent.Id] =
    self.arg(ExecutionEventSchema.OptionalExecutionEventIdArgument)

  def observationId: Observation.Id =
    self.arg(ObservationSchema.ObservationIdArgument)

  def optionalObservationId: Option[Observation.Id] =
    self.arg(ObservationSchema.OptionalObservationIdArgument)

  def programId: Program.Id =
    self.arg(ProgramSchema.ProgramIdArgument)

  def optionalProgramId: Option[Program.Id] =
    self.arg(ProgramSchema.OptionalProgramIdArgument)

  def stepId: Step.Id =
    self.arg(StepSchema.ArgumentStepId)

  def optionalStepId: Option[Step.Id] =
    self.arg(StepSchema.ArgumentOptionalStepId)

  def targetId: Target.Id =
    self.arg(TargetSchema.ArgumentTargetId)

  def optionalTargetId: Option[Target.Id] =
    self.arg(TargetSchema.ArgumentOptionalTargetId)

  def visitId: Visit.Id =
    self.arg(VisitRecordSchema.ArgumentVisitId)

  def includeDeleted: Boolean =
    self.arg(GeneralSchema.ArgumentIncludeDeleted)

  def resultSetLimit: Option[NonNegInt] =
    self
      .arg(QuerySchema.ArgumentOptionLimit)
      .orElse(QuerySchema.DefaultLimit.some)
      .map { lim => if (lim.value > QuerySchema.DefaultLimit.value) QuerySchema.DefaultLimit else lim }

  def unsafeToFuture[B](fb: F[B])(implicit ev: Dispatcher[F]): Future[B] =
    implicitly[Dispatcher[F]].unsafeToFuture(fb)

  def dataset[B](f: DatasetRepo[F] => F[B])(implicit ev: Dispatcher[F]): Future[B]=
    unsafeToFuture(f(self.ctx.odbRepo.dataset))

  def executionEvent[B](f: ExecutionEventRepo[F] => F[B])(implicit ev: Dispatcher[F]): Future[B] =
    unsafeToFuture(f(self.ctx.odbRepo.executionEvent))

  def observation[B](f: ObservationRepo[F] => F[B])(implicit ev: Dispatcher[F]): Future[B] =
    unsafeToFuture(f(self.ctx.odbRepo.observation))

  def program[B](f: ProgramRepo[F] => F[B])(implicit ev: Dispatcher[F]): Future[B] =
    unsafeToFuture(f(self.ctx.odbRepo.program))

  def target[B](f: TargetRepo[F] => F[B])(implicit ev: Dispatcher[F]): Future[B] =
    unsafeToFuture(f(self.ctx.odbRepo.target))

}

trait ToRepoContextOps {
  implicit def toRepoContextOps[F[_]](self: Context[OdbCtx[F], _]): OdbContextOps[F] =
    new OdbContextOps[F](self)
}

object context extends ToRepoContextOps

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen

import cats.{Eq, Functor}
import cats.syntax.eq._
import cats.syntax.functor._
import lucuma.core.model.Observation
import lucuma.odb.api.model.{StepConfig, StepQaState, Visit, VisitRecord, VisitRecords}
import lucuma.odb.api.repo.OdbRepo

/**
 * RecordedStep is just the subset of StepRecord.Output that we're interested
 * in for the purpose of generating a sequence.
 */
final case class RecordedStep[D](
  stepConfig:           StepConfig[D],
  successfullyExecuted: Boolean,
) {

  def toTuple: (StepConfig[D], Boolean) =
    (stepConfig, successfullyExecuted)
}

object RecordedStep {

  implicit def EqRecordedStep[D: Eq]: Eq[RecordedStep[D]] =
    Eq.by { a => (
      a.stepConfig,
      a.successfullyExecuted,
    )}

  def lookup[F[_]: Functor, S, D](
    odb:    OdbRepo[F],
    oid:    Observation.Id,
    visits: VisitRecords => List[(Visit.Id, VisitRecord[S, D])]
  ): F[List[RecordedStep[D]]] =
    odb
      .executionEvent
      .selectStepsForObservation(oid, visits)
      .map(_.map(r => RecordedStep(r.stepConfig, r.isExecuted && r.qaState.forall(_ === StepQaState.Pass)))) // assume success before QA

}

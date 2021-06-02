// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.model.{Atom, Observation, Step}
import cats.{Eq, Functor}
import cats.mtl.Stateful
import io.chrisdavenport.cats.time._

import java.time.Instant

final case class ExecutedStepModel(
  stepId:        Step.Id,
  atomId:        Atom.Id,
  observationId: Observation.Id,
  stepEvents:    List[ExecutionEventModel.StepEvent],
  datasetEvents: List[ExecutionEventModel.DatasetEvent],
  datasets:      List[DatasetModel],
  startTime:     Instant,
  endTime:       Instant
) {

  def sequenceType: SequenceModel.SequenceType =
    stepEvents.map(_.sequenceType).distinct match {
      case List(t) => t
      case _       => SequenceModel.SequenceType.Science
    }

  def dereference[F[_]: Functor, T, D](
    db: DatabaseReader[T]
  )(
    f: StepConfig[_] => Option[D]
  )(
    implicit S: Stateful[F, T]
  ): F[Option[StepModel[D]]] =

    StepModel.dereference[F, T, D](db, stepId)(f)

}

object ExecutedStepModel {

  implicit val EqExecutedStep: Eq[ExecutedStepModel] =
    Eq.by { a => (
      a.stepId,
      a.atomId,
      a.observationId,
      a.sequenceType,
      a.stepEvents,
      a.datasetEvents,
      a.datasets,
      a.startTime,
      a.endTime
    )}

}

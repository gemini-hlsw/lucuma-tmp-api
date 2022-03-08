// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.data.StateT
import lucuma.core.model.{Atom, Observation, Step}
import cats.Eq
import org.typelevel.cats.time._

import java.time.Instant

/**
 * Information associated with an executed step.
 *
 * @param stepId corresponding step id
 * @param atomId atom in which the step appears
 * @param observationId observation in which the step appears
 * @param stepEvents step events tied to this step
 * @param datasetEvents dataset events associated with this step
 * @param datasets all the datasets produced by this step
 * @param startTime when the step started
 * @param endTime when the step finished
 */
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

  /**
   * Whether this is an acquisition or science step.
   */
  def sequenceType: SequenceModel.SequenceType =
    stepEvents.map(_.sequenceType).distinct match {
      case List(t) => t
      case _       => SequenceModel.SequenceType.Science
    }

  /**
   * Extracts the step configuration.
   */
  def dereference[D](
    f: StepConfig[_] => Option[D]
  ): StateT[EitherInput, Database, Option[StepModel[D]]] =

    StepModel.dereference[D](stepId)(f)

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

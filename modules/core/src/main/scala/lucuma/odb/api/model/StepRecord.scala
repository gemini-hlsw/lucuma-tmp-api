// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.syntax.eq._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.core.model.Observation
import lucuma.odb.api.model.time.NonNegDuration
import monocle.{Focus, Lens}
import org.typelevel.cats.time.instances.instant._

import java.time.Instant

/**
 * A record of an executed step.  StepRecord is expected to be created by
 * Observe before it executes a step.  It contains the instrument and step-type
 * configuration.  An ID is associated with the step and this ID must be
 * included in subsequent step and dataset events.
 *
 * @tparam D dynamic instrument configuration type (eg, GmosModel.GmosNorthDynamic)
 */
final case class StepRecord[D](
  created:      Instant,
  stepConfig:   StepConfig[D]
)

object StepRecord {

  implicit def EqStepRecord[D: Eq]: Eq[StepRecord[D]] =
    Eq.by { a => (
      a.created,
      a.stepConfig
    )}


  final case class Input[DI](
    observationId: Observation.Id,
    visitId:       Visit.Id,
    stepConfig:    StepConfig.CreateStepConfig[DI]
  ) {

    def create[D](when: Instant)(implicit ev: InputValidator[DI, D]): ValidatedInput[StepRecord[D]] =
      stepConfig.create[D].map(s => StepRecord(when, s))

  }

  object Input {

    implicit def DecoderInput[DI: Decoder]: Decoder[Input[DI]] =
      deriveDecoder[Input[DI]]

    implicit def EqInput[DI: Eq]: Eq[Input[DI]] =
      Eq.by { a => (
        a.observationId,
        a.visitId,
        a.stepConfig
      )}

  }

  final case class Output[D](
    observationId: Observation.Id,
    visitId:       Visit.Id,
    stepId:        Step.Id,
    created:       Instant,
    stepConfig:    StepConfig[D],
    stepEvents:    List[ExecutionEventModel.StepEvent],
    datasetEvents: List[ExecutionEventModel.DatasetEvent],
    datasets:      List[DatasetModel]
  ) {

    def startTime: Option[Instant] =
      stepEvents.headOption.map(_.received)

    def endTime:   Option[Instant] =
      stepEvents.lastOption.map(_.received)

    def duration:  NonNegDuration =
      (
        for {
          s <- startTime
          e <- endTime
        } yield NonNegDuration.between(s, e)
      ).getOrElse(NonNegDuration.zero)

    def isExecuted: Boolean =
      stepEvents.exists(_.payload.stage === ExecutionEventModel.StepStageType.EndStep)

    def qaState: Option[StepQaState] =
      StepQaState.rollup(datasets.map(_.dataset.qaState))

    /**
     * Whether this is an acquisition or science step.
     */
    def sequenceType: SequenceModel.SequenceType =
      stepEvents.map(_.payload.sequenceType).distinct match {
        case List(t) => t
        case _       => SequenceModel.SequenceType.Science
      }

  }

  object Output {

    def init[D](
      observationId: Observation.Id,
      visitId:       Visit.Id,
      stepId:        Step.Id,
      created:       Instant,
      stepConfig:    StepConfig[D]
    ): Output[D] =
      Output(observationId, visitId, stepId, created, stepConfig, Nil, Nil, Nil)

    implicit def EqOutput[D: Eq]: Eq[Output[D]] =
      Eq.by { a => (
        a.observationId,
        a.visitId,
        a.stepId,
        a.created,
        a.stepConfig,
        a.sequenceType,
        a.stepEvents,
        a.datasetEvents,
        a.datasets
      )}

    def stepEvents[D]: Lens[Output[D], List[ExecutionEventModel.StepEvent]] =
      Focus[Output[D]](_.stepEvents)

    def datasetEvents[D]: Lens[Output[D], List[ExecutionEventModel.DatasetEvent]] =
      Focus[Output[D]](_.datasetEvents)

    def datasets[D]: Lens[Output[D], List[DatasetModel]] =
      Focus[Output[D]](_.datasets)

  }

}

trait StepRecordOptics { self: StepRecord.type =>

  def created[D]: Lens[StepRecord[D], Instant] =
    Focus[StepRecord[D]](_.created)

  def stepConfig[D]: Lens[StepRecord[D], StepConfig[D]] =
    Focus[StepRecord[D]](_.stepConfig)

}

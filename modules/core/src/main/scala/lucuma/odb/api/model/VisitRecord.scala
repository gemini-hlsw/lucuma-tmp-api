// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.data.Ior
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.core.model.Observation
import lucuma.odb.api.model.ExecutionEventModel.SequenceEvent
import lucuma.odb.api.model.syntax.inputvalidator._
import monocle.{Focus, Lens}
import org.typelevel.cats.time.instances.instant._

import java.time.{Duration, Instant}

import scala.collection.immutable.ListMap
import scala.concurrent.duration._
import scala.math.Ordering.Implicits.infixOrderingOps

/**
 * A VisitRecord is expected to be created by Observe before observing (a
 * portion of) an observation. It contains the static configuration that will
 * apply throughout the visit.  Observe creates the visit and thereby obtains a
 * visit id, which must be supplied in subsequent events.
 */
final case class VisitRecord[S, D](
  created:       Instant,
  static:        S,
  steps:         ListMap[Step.Id, StepRecord[D]]
)

object VisitRecord extends VisitRecordOptics {

  implicit def EqExecRecord[S: Eq, D: Eq]: Eq[VisitRecord[S, D]] =
    Eq.by { a => (
      a.created,
      a.static,
      a.steps.values.toList
    )}

  final case class Input[SI](
    observationId: Observation.Id,
    static:        SI
  ) {

    def create[S, D](when: Instant)(implicit V: InputValidator[SI, S]): ValidatedInput[VisitRecord[S, D]] =
      static.validateAndCreate.map { s => VisitRecord(when, s, ListMap.empty) }

  }

  object Input {

    implicit def EqInput[SI: Eq]: Eq[Input[SI]] =
      Eq.by { a => (
        a.observationId,
        a.static
     ) }

    implicit def DecoderInput[SI: Decoder]: Decoder[Input[SI]] =
      deriveDecoder[Input[SI]]

  }

  final case class Output[S, D](
    observationId:  Observation.Id,
    visitId:        Visit.Id,
    created:        Instant,
    staticConfig:   S,
    steps:          List[StepRecord.Output[D]],
    sequenceEvents: List[ExecutionEventModel.SequenceEvent]
  ) {

    def startTime: Option[Instant] =
      Ior
        .fromOptions(sequenceEvents.headOption.map(_.received), steps.headOption.flatMap(_.startTime))
        .map(_.fold(identity, identity, _ min _))

    def endTime: Option[Instant] =
      Ior
        .fromOptions(sequenceEvents.lastOption.map(_.received), steps.lastOption.flatMap(_.endTime))
        .map(_.fold(identity, identity, _ max _))

    def duration: FiniteDuration =
      (for {
        s <- startTime
        e <- endTime
      } yield {
        val d = Duration.between(s, e)
        (d.getSeconds * 1000000000 + d.getNano).nanoseconds
      }).getOrElse(0.nanoseconds)

  }

  object Output {

    def init[S, D](
      observationId: Observation.Id,
      visitId:       Visit.Id,
      created:       Instant,
      staticConfig:  S
    ): Output[S, D] =
      Output(observationId, visitId, created, staticConfig, Nil, Nil)

    implicit def EqOutput[S: Eq, D: Eq]: Eq[Output[S, D]] =
      Eq.by { a => (
        a.observationId,
        a.visitId,
        a.created,
        a.staticConfig,
        a.steps,
        a.sequenceEvents
      )}

    def staticConfig[S, D]: Lens[Output[S, D], S] =
      Focus[Output[S, D]](_.staticConfig)

    def steps[S, D]: Lens[Output[S, D], List[StepRecord.Output[D]]] =
      Focus[Output[S, D]](_.steps)

    def sequenceEvents[S, D]: Lens[Output[S, D], List[SequenceEvent]] =
      Focus[Output[S, D]](_.sequenceEvents)

  }

}

trait VisitRecordOptics { self: VisitRecord.type =>

  def created[S, D]: Lens[VisitRecord[S, D], Instant] =
    Focus[VisitRecord[S, D]](_.created)

  def static[S, D]: Lens[VisitRecord[S, D], S] =
    Focus[VisitRecord[S, D]](_.static)

  def steps[S, D]: Lens[VisitRecord[S, D], ListMap[Step.Id, StepRecord[D]]] =
    Focus[VisitRecord[S, D]](_.steps)

}
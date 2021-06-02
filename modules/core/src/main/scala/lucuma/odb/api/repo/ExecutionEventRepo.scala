// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{DatabaseState, DatasetModel, ExecutedStepModel, ExecutionEvent, ExecutionEventModel, InputError, ValidatedInput}
import lucuma.odb.api.model.ExecutionEventModel.{DatasetEvent, SequenceEvent, StepEvent}
import lucuma.core.model.{Atom, Observation, Step}
import cats.data.{EitherT, State}
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all._
import cats.effect.Sync
import cats.effect.concurrent.Ref
import eu.timepit.refined.cats._
import eu.timepit.refined.types.all.PosInt

import java.time.Instant

import scala.collection.immutable.SortedMap


sealed trait ExecutionEventRepo[F[_]] {

  def selectEvent(
    eid: ExecutionEvent.Id
  ): F[Option[ExecutionEventModel]]

  def selectPageForObservation(
    oid:      Observation.Id,
    count:    Option[Int],
    afterGid: Option[ExecutionEvent.Id] = None
  ): F[ResultPage[ExecutionEventModel]]

  def selectDatasetsForObservation(
    oid:   Observation.Id,
    count: Int,
    after: Option[(Step.Id, PosInt)] = None
  ): F[ResultPage[DatasetModel]]

  def selectExecutedStepsForObservation(
    oid: Observation.Id,
    count: Int,
    after: Option[Step.Id] = None
  ): F[ResultPage[ExecutedStepModel]]

  def insertSequenceEvent(
    event: SequenceEvent.Add
  ): F[SequenceEvent]

  def insertStepEvent(
    event: StepEvent.Add
  ): F[StepEvent]

  def insertDatasetEvent(
    event: DatasetEvent.Add
  ): F[DatasetEvent]

}

object ExecutionEventRepo {

  private final case class EventsPair(
    datasetEvents: List[DatasetEvent],
    stepEvents:    List[StepEvent]
  ) {

    def addDatasetEvent(de: DatasetEvent): EventsPair =
      copy(datasetEvents = de :: datasetEvents)

    def addStepEvent(se: StepEvent): EventsPair =
      copy(stepEvents = se :: stepEvents)

    def isExecuted: Boolean =
      stepEvents.exists(_.stage === ExecutionEventModel.StepStageType.EndStep)

  }

  private object EventsPair {
    def fromDatasetEvent(de: DatasetEvent): EventsPair =
      EventsPair(List(de), Nil)

    def fromStepEvent(se: StepEvent): EventsPair =
      EventsPair(Nil, List(se))
  }

  def create[F[_]: Sync](
    tablesRef: Ref[F, Tables]
  ): ExecutionEventRepo[F] =
    new ExecutionEventRepo[F] {

      override def selectEvent(
        eid: ExecutionEvent.Id
      ): F[Option[ExecutionEventModel]] =
        tablesRef.get.map(Tables.executionEvent(eid).get)

      private def sortedEvents(oid: Observation.Id): F[List[ExecutionEventModel]] =
        tablesRef.get.map(
          _.executionEvents
           .values
           .filter(_.observationId === oid)
           .toList
           .sortBy(e => (e.generated, e.id))
        )

      private val stepAtoms: F[Map[Step.Id, Atom.Id]] =
        tablesRef.get.map(
          _.atoms.values.foldLeft(Map.empty[Step.Id, Atom.Id]) { (m, a) =>
            a.steps.foldLeft(m) { (m2, sid) => m2 + (sid -> a.id) }
          }
        )

      private def executedSteps(oid: Observation.Id): F[List[ExecutedStepModel]] =
        for {
          as <- stepAtoms
          es <- sortedEvents(oid)
        } yield  {

          val byStep = es.foldRight(SortedMap.empty[Step.Id, EventsPair]) { (e, m) =>
            e match {
              case _: SequenceEvent => m
              case d: DatasetEvent  => m.updatedWith(d.stepId)(_.fold(EventsPair.fromDatasetEvent(d))(_.addDatasetEvent(d)).some)
              case s: StepEvent     => m.updatedWith(s.stepId)(_.fold(EventsPair.fromStepEvent(s))(_.addStepEvent(s)).some)
            }
          }

          byStep.toList.collect { case (sid, ep: EventsPair) if ep.isExecuted =>
            ExecutedStepModel(
              sid,
              as(sid),
              oid,
              ep.stepEvents,
              ep.datasetEvents,
              ep.datasetEvents.flatMap(_.toDataset).distinct,
              ep.stepEvents.head.generated,
              ep.stepEvents.last.generated
            )
          }.sortBy(s => (s.endTime, s.stepId))

        }

      override def selectEventsForObservation(
        oid:      Observation.Id,
        count:    Option[Int],
        afterGid: Option[ExecutionEvent.Id]
      ): F[ResultPage[ExecutionEventModel]] =
        sortedEvents(oid).map(ResultPage.fromSeq(_, count, afterGid, _.id))

      override def selectDatasetsForObservation(
        oid:   Observation.Id,
        count: Int,
        after: Option[(Step.Id, PosInt)] = None
      ): F[ResultPage[DatasetModel]] =

        tablesRef.get.map { tables =>
          tables.executionEvents.values.collect {
            case de: DatasetEvent if de.observationId === oid => de.toDataset
          }.toList.flattenOption.distinct.sortBy(dm => (dm.stepId, dm.index))
        }.map { all =>

          ResultPage.fromSeq(
            all,
            count,
            after,
            dm => (dm.stepId, dm.index)
          )

        }

      def selectExecutedStepsForObservation(
        oid:   Observation.Id,
        count: Int,
        after: Option[Step.Id] = None
      ): F[ResultPage[ExecutedStepModel]] = {

        executedSteps(oid).map { steps =>

          ResultPage.fromSeq(
            steps,
            count,
            after,
            _.stepId
          )

        }

      }

      private def received: F[Instant] =
        Sync[F].delay(Instant.now)

      private def runState[T](
        s: State[Tables, ValidatedInput[T]]
      ): F[T] =
        EitherT(
          tablesRef.modify { tables =>
            val (tablesʹ, e) = s.run(tables).value
            e.fold(
              err => (tables, InputError.Exception(err).asLeft),
              evt => (tablesʹ, evt.asRight)
            )
          }
        ).rethrowT

      private def insertEvent[A](
        f: (DatabaseState[Tables], Instant) => State[Tables, ValidatedInput[A]]
      ): F[A] =
        for {
          w <- received
          e <- runState(f(TableState, w))
        } yield e

      override def insertSequenceEvent(
        event: SequenceEvent.Add
      ): F[SequenceEvent] =
        insertEvent(event.add[State[Tables, *], Tables])

      override def insertStepEvent(
        event: StepEvent.Add
      ): F[StepEvent] =
        insertEvent(event.add[State[Tables, *], Tables])

      override def insertDatasetEvent(
        event: DatasetEvent.Add
      ): F[DatasetEvent] =
        insertEvent(event.add[State[Tables, *], Tables])

    }


}

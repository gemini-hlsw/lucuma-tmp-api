// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.MonadError
import lucuma.odb.api.model.{AtomModel, DatabaseState, DatasetModel, ExecutedStepModel, ExecutionEventModel, InputError, SequenceModel, ValidatedInput}
import lucuma.odb.api.model.ExecutionEventModel.{DatasetEvent, SequenceEvent, StepEvent}
import lucuma.core.model.{Atom, ExecutionEvent, Observation, Step}
import cats.data.{EitherT, State}
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all._
import cats.effect.{Clock, Ref}
import eu.timepit.refined.cats._
import eu.timepit.refined.types.all.PosInt

import java.time.Instant

import scala.collection.immutable.SortedMap


sealed trait ExecutionEventRepo[F[_]] {

  def selectEvent(
    eid: ExecutionEvent.Id
  ): F[Option[ExecutionEventModel]]

  /** Page events associated with an observation */
  def selectEventsPageForObservation(
    oid:      Observation.Id,
    count:    Option[Int],
    afterGid: Option[ExecutionEvent.Id] = None
  ): F[ResultPage[ExecutionEventModel]]

  /** Page datasets associated with an observation  */
  def selectDatasetsPageForObservation(
    oid:   Observation.Id,
    count: Option[Int],
    after: Option[(Step.Id, PosInt)] = None
  ): F[ResultPage[DatasetModel]]

  /** Page datasets associated with an individual step. */
  def selectDatasetsPageForStep(
    sid:   Step.Id,
    count: Option[Int],
    after: Option[PosInt] = None
  ): F[ResultPage[DatasetModel]]

  /** Page executed steps associated with an observation. */
  def selectExecutedStepsPageForObservation(
    oid:   Observation.Id,
    count: Option[Int],
    after: Option[Step.Id] = None
  ): F[ResultPage[ExecutedStepModel]]

  /** Select all executed steps for an observation at once. */
  def selectExecutedStepsForObservation(
    oid: Observation.Id
  ): F[List[ExecutedStepModel]]

  /**
   * Select all not-completely-executed atoms after the last executed atom. A
   * step is considered executed if we've received an end step event.  An atom
   * is executed if all steps are executed.
   */
  def selectRemainingAtoms(
    oid:     Observation.Id,
    seqType: SequenceModel.SequenceType
  ): F[List[AtomModel[Step.Id]]]

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

  /**
   * Groups step and dataset events associated with a particular step.
   */
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

  def create[F[_]: Clock](
    tablesRef: Ref[F, Tables]
  )(implicit E: MonadError[F, Throwable]): ExecutionEventRepo[F] =
    new ExecutionEventRepo[F] {

      override def selectEvent(
        eid: ExecutionEvent.Id
      ): F[Option[ExecutionEventModel]] =
        tablesRef.get.map(Tables.executionEvent(eid).get)

      // Sort events by generation timestamp + event id
      private def sortedEvents(tables: Tables, oid: Observation.Id): List[ExecutionEventModel] =
        tables
         .executionEvents
         .values
         .filter(_.observationId === oid)
         .toList
         .sortBy(e => (e.generated, e.id))


      // Map of steps to the atoms that contain them.
      private def stepAtoms(tables: Tables): Map[Step.Id, Atom.Id] =
        tables.atoms.values.foldLeft(Map.empty[Step.Id, Atom.Id]) { (m, a) =>
          a.steps.foldLeft(m) { (m2, sid) => m2 + (sid -> a.id) }
        }

      private def executedStepsForObservation(tables: Tables, oid: Observation.Id): List[ExecutedStepModel] = {
        val as = stepAtoms(tables)
        val es = sortedEvents(tables, oid)

        // Steps and their associated dataset and step events.
        val byStep = es.foldRight(SortedMap.empty[Step.Id, EventsPair]) { (e, m) =>
          e match {
            case _: SequenceEvent => m
            case d: DatasetEvent  => m.updatedWith(d.stepId)(_.fold(EventsPair.fromDatasetEvent(d))(_.addDatasetEvent(d)).some)
            case s: StepEvent     => m.updatedWith(s.stepId)(_.fold(EventsPair.fromStepEvent(s))(_.addStepEvent(s)).some)
          }
        }

        // Create executed steps from the event information.
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

      override def selectExecutedStepsForObservation(oid: Observation.Id): F[List[ExecutedStepModel]] =
        tablesRef.get.map(executedStepsForObservation(_, oid))

      override def selectEventsPageForObservation(
        oid:      Observation.Id,
        count:    Option[Int],
        afterGid: Option[ExecutionEvent.Id]
      ): F[ResultPage[ExecutionEventModel]] =
        tablesRef.get.map { tables =>
          ResultPage.fromSeq(sortedEvents(tables, oid), count, afterGid, _.id)
        }

      override def selectDatasetsPageForObservation(
        oid:   Observation.Id,
        count: Option[Int],
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

      override def selectDatasetsPageForStep(
        sid:   Step.Id,
        count: Option[Int],
        after: Option[PosInt] = None
      ): F[ResultPage[DatasetModel]] =

        tablesRef.get.map { tables =>
          tables.executionEvents.values.collect {
            case de: DatasetEvent if de.stepId === sid => de.toDataset
          }.toList.flattenOption.distinct.sortBy(_.index)
        }.map { all =>

          ResultPage.fromSeq(
            all,
            count,
            after,
            _.index
          )

        }


      override def selectExecutedStepsPageForObservation(
        oid:   Observation.Id,
        count: Option[Int],
        after: Option[Step.Id] = None
      ): F[ResultPage[ExecutedStepModel]] = {

        selectExecutedStepsForObservation(oid).map { steps =>

          ResultPage.fromSeq(
            steps,
            count,
            after,
            _.stepId
          )

        }

      }

      private def remainingAtoms(
        tables:   Tables,
        oid:      Observation.Id,
        seqType:  SequenceModel.SequenceType,
        executed: List[ExecutedStepModel]
      ): List[AtomModel[Step.Id]] = {

        val isExecutedStep: Set[Step.Id] =
          executed.map(_.stepId).toSet

        tables
          .observations
          .get(oid)
          .flatMap(_.config)
          .map { ref =>
            seqType match {
              case SequenceModel.SequenceType.Acquisition => ref.acquisition
              case SequenceModel.SequenceType.Science     => ref.science
            }
          }
          .toList
          .flatMap { seq =>
            seq
              .atoms
              .map(tables.atoms(_))
              .filter(a => !a.steps.forall(isExecutedStep))
          }

      }

      def selectRemainingAtoms(
        oid: Observation.Id,
        seqType: SequenceModel.SequenceType
      ): F[List[AtomModel[Step.Id]]] =
        tablesRef.get.map { tables =>
          remainingAtoms(tables, oid, seqType, executedStepsForObservation(tables, oid))
        }

      private def received: F[Instant] =
        Clock[F].realTime.map(d => Instant.ofEpochMilli(d.toMillis))

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

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.MonadError
import lucuma.odb.api.model.{AtomModel, Database, DatasetModel, EitherInput, ExecutedStepModel, ExecutionEventModel, SequenceModel}
import lucuma.odb.api.model.ExecutionEventModel.{DatasetEvent, SequenceEvent, StepEvent}
import lucuma.odb.api.model.syntax.databasestate._
import lucuma.odb.api.model.syntax.eitherinput._
import lucuma.core.model.{Atom, ExecutionEvent, Observation, Step}
import cats.data.StateT
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
    databaseRef: Ref[F, Database]
  )(implicit E: MonadError[F, Throwable]): ExecutionEventRepo[F] =
    new ExecutionEventRepo[F] {

      override def selectEvent(
        eid: ExecutionEvent.Id
      ): F[Option[ExecutionEventModel]] =
        databaseRef.get.map(_.executionEvents.rows.get(eid))

      // Sort events by generation timestamp + event id
      private def sortedEvents(db: Database, oid: Observation.Id): List[ExecutionEventModel] =
        db
         .executionEvents
         .rows
         .values
         .filter(_.observationId === oid)
         .toList
         .sortBy(e => (e.generated, e.id))


      // Map of steps to the atoms that contain them.
      private def stepAtoms(db: Database): Map[Step.Id, Atom.Id] =
        db.atoms.rows.values.foldLeft(Map.empty[Step.Id, Atom.Id]) { (m, a) =>
          a.steps.foldLeft(m) { (m2, sid) => m2 + (sid -> a.id) }
        }

      private def executedStepsForObservation(db: Database, oid: Observation.Id): List[ExecutedStepModel] = {
        val as = stepAtoms(db)
        val es = sortedEvents(db, oid)

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
        databaseRef.get.map(executedStepsForObservation(_, oid))

      override def selectEventsPageForObservation(
        oid:      Observation.Id,
        count:    Option[Int],
        afterGid: Option[ExecutionEvent.Id]
      ): F[ResultPage[ExecutionEventModel]] =
        databaseRef.get.map { tables =>
          ResultPage.fromSeq(sortedEvents(tables, oid), count, afterGid, _.id)
        }

      override def selectDatasetsPageForObservation(
        oid:   Observation.Id,
        count: Option[Int],
        after: Option[(Step.Id, PosInt)] = None
      ): F[ResultPage[DatasetModel]] =

        databaseRef.get.map { db =>
          db.executionEvents.rows.values.collect {
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

        databaseRef.get.map { db =>
          db.executionEvents.rows.values.collect {
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
        db:       Database,
        oid:      Observation.Id,
        seqType:  SequenceModel.SequenceType,
        executed: List[ExecutedStepModel]
      ): List[AtomModel[Step.Id]] = {

        val isExecutedStep: Set[Step.Id] =
          executed.map(_.stepId).toSet

        db
          .observations
          .rows
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
              .map(db.atoms.rows(_))
              .filter(a => !a.steps.forall(isExecutedStep))
          }

      }

      def selectRemainingAtoms(
        oid: Observation.Id,
        seqType: SequenceModel.SequenceType
      ): F[List[AtomModel[Step.Id]]] =
        databaseRef.get.map { tables =>
          remainingAtoms(tables, oid, seqType, executedStepsForObservation(tables, oid))
        }

      private def received: F[Instant] =
        Clock[F].realTime.map(d => Instant.ofEpochMilli(d.toMillis))


      private def insertEvent[A](
        f: Instant => StateT[EitherInput, Database, A]
      ): F[A] =
        for {
          w <- received
          e <- databaseRef.modifyState(f(w).flipF).flatMap(_.liftTo[F])
        } yield e

      override def insertSequenceEvent(
        event: SequenceEvent.Add
      ): F[SequenceEvent] =
        insertEvent(event.add)

      override def insertStepEvent(
        event: StepEvent.Add
      ): F[StepEvent] =
        insertEvent(event.add)

      override def insertDatasetEvent(
        event: DatasetEvent.Add
      ): F[DatasetEvent] =
        insertEvent(event.add)

    }


}

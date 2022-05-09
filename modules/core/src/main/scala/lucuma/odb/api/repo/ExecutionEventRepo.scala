// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.data.StateT
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all._
import cats.effect.{Clock, Ref, Sync}
import lucuma.core.`enum`.Instrument
import lucuma.odb.api.model.{Database, DatasetModel, DatasetTable, EitherInput, ExecutionEventModel, InputValidator, Step, StepRecord, ValidatedInput, Visit, VisitRecord, VisitRecords}
import lucuma.odb.api.model.ExecutionEventModel.{DatasetEvent, SequenceEvent, StepEvent}
import lucuma.odb.api.model.syntax.databasestate._
import lucuma.odb.api.model.syntax.eitherinput._
import lucuma.odb.api.model.syntax.lens._
import lucuma.odb.api.model.syntax.optional._
import lucuma.core.model.{ExecutionEvent, Observation}
import monocle.Prism

import java.time.Instant

import scala.collection.immutable.ListMap

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

  def selectStepEventsPageForObservation(
    oid:      Observation.Id,
    count:    Option[Int],
    afterGid: Option[ExecutionEvent.Id] = None
  ): F[ResultPage[StepEvent]]

  def selectDatasetEventsPageForObservation(
    oid:      Observation.Id,
    count:    Option[Int],
    afterGid: Option[ExecutionEvent.Id] = None
  ): F[ResultPage[DatasetEvent]]

  def selectStepForId[S, D](
    oid:    Observation.Id,
    stepId: Step.Id,
    visits: VisitRecords => List[(Visit.Id, VisitRecord[S, D])]
  ): F[Option[StepRecord.Output[D]]]

  /** Page executed visits associated with an observation. */
  def selectVisitsPageForObservation[S, D](
    oid:    Observation.Id,
    visits: VisitRecords => List[(Visit.Id, VisitRecord[S, D])],
    count:  Option[Int],
    after:  Option[Visit.Id] = None
  ): F[ResultPage[VisitRecord.Output[S, D]]]

  /** Page executed steps associated with an observation. */
  def selectStepsPageForObservation[S, D](
    oid:    Observation.Id,
    visits: VisitRecords => List[(Visit.Id, VisitRecord[S, D])],
    count:  Option[Int],
    after:  Option[Step.Id] = None
  ): F[ResultPage[StepRecord.Output[D]]]

  /** Select all recorded steps for an observation at once. */
  def selectStepsForObservation[S, D](
    oid:    Observation.Id,
    visits: VisitRecords => List[(Visit.Id, VisitRecord[S, D])]
  ): F[List[StepRecord.Output[D]]]

  def selectExistentialStepsForObservation(
    oid:  Observation.Id,
   ): F[List[StepRecord.Output[_]]]

  def insertVisit[SI, S, D](
    visitId:       Visit.Id,
    visit:         VisitRecord.Input[SI],
    prism:         Prism[VisitRecords, ListMap[Visit.Id, VisitRecord[S, D]]]
  )(implicit ev: InputValidator[SI, S]): F[VisitRecord.Output[S, D]]

  def insertStep[DI, S, D](
    stepId: Step.Id,
    step:   StepRecord.Input[DI],
    prism:  Prism[VisitRecords, ListMap[Visit.Id, VisitRecord[S, D]]]
  )(implicit ev: InputValidator[DI, D]): F[StepRecord.Output[D]]

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

  def create[F[_]: Sync](
    databaseRef: Ref[F, Database]
  ): ExecutionEventRepo[F] =
    new ExecutionEventRepo[F] {

      override def selectEvent(
        eid: ExecutionEvent.Id
      ): F[Option[ExecutionEventModel]] =
        databaseRef.get.map(_.executionEvents.rows.get(eid))

      // Sort events by timestamp + event id

      private def sortedEvents(
        db: Database
      )(
        p: ExecutionEventModel => Boolean
      ): List[ExecutionEventModel] =
        db
         .executionEvents
         .rows
         .values
         .filter(p)
         .toList
         .sortBy(e => (e.received, e.id))

      private def addEventsToSteps[D](
        oid:    Observation.Id,
        events: List[ExecutionEventModel],
        recs:   List[(Visit.Id, (Step.Id, StepRecord[D]))],
        dsets:  DatasetTable
      ): List[StepRecord.Output[D]] = {

        val stepMap = recs.map { case (vid, (sid, r)) =>
          sid -> StepRecord.Output.init(oid, vid, sid, r.created, r.stepConfig)
        }.toMap

        // Steps and their associated dataset and step events.
        val byStep = events.foldRight(stepMap) { (e, m) =>
          e match {
            case _: SequenceEvent => m
            case d: DatasetEvent  => m.updatedWith(d.location.stepId)(_.map(x => StepRecord.Output.datasetEvents.modify(_ :+ d)(x)))
            case s: StepEvent     => m.updatedWith(s.stepId)(_.map(x => StepRecord.Output.stepEvents.modify(_ :+ s)(x)))
          }
        }

        byStep
          .values
          .toList
          .sortBy(s => (s.startTime, s.stepId))
          .map(s => StepRecord.Output.datasets.replace(dsets.selectAll(oid, s.stepId.some, None))(s))
      }

      private def recordedStep[S, D](
        db:     Database,
        oid:    Observation.Id,
        stepId: Step.Id,
        visits: VisitRecords => List[(Visit.Id, VisitRecord[S, D])]
      ): Option[StepRecord.Output[D]] = {

        val events  = sortedEvents(db) {
          case SequenceEvent(_, o, _, _, _)                       => o === oid
          case StepEvent(_, o, _, s, _, _, _)                     => o === oid && s === stepId
          case DatasetEvent(_, _, _, DatasetModel.Id(o, s, _), _) => o === oid && s === stepId
        }

        val stepRec = Database.visitRecordsAt(oid).get(db).toList.flatMap { vrs =>
          visits(vrs).flatMap { case (vid, vr) =>
            vr.steps.get(stepId).tupleLeft(stepId).toList.tupleLeft(vid)
          }
        }

        addEventsToSteps(oid, events, stepRec, db.datasets).headOption

      }

      override def selectStepForId[S, D](
        oid:    Observation.Id,
        stepId: Step.Id,
        visits: VisitRecords => List[(Visit.Id, VisitRecord[S, D])]
      ): F[Option[StepRecord.Output[D]]] =
        databaseRef.get.map(recordedStep(_, oid, stepId, visits))

      private def recordedSteps[S, D](
        db:     Database,
        oid:    Observation.Id,
        visits: List[(Visit.Id, VisitRecord[S, D])]
      ): List[StepRecord.Output[D]] = {

        val events   = sortedEvents(db)(_.observationId === oid)
        val stepRecs = visits.flatMap { case (vid, vr) => vr.steps.toList.tupleLeft(vid) }
        addEventsToSteps(oid, events, stepRecs, db.datasets)
      }

      private def recordedSteps[S, D](
        db:     Database,
        oid:    Observation.Id,
        visits: VisitRecords => List[(Visit.Id, VisitRecord[S, D])]
      ): List[StepRecord.Output[D]] =
        recordedSteps[S, D](db, oid, Database.visitRecordsAt(oid).get(db).toList.flatMap(visits))

      override def selectStepsForObservation[S, D](
        oid:    Observation.Id,
        visits: VisitRecords => List[(Visit.Id, VisitRecord[S, D])]
      ): F[List[StepRecord.Output[D]]] =
        databaseRef.get.map(recordedSteps(_, oid, visits))

      override def selectExistentialStepsForObservation(
        oid:  Observation.Id,
      ): F[List[StepRecord.Output[_]]] =
        databaseRef.get.map { db =>
          db.observations.rows.get(oid).flatMap(_.config).map(_.instrument match {
            // How do you do this without breaking out the various cases?
            case Instrument.GmosNorth => recordedSteps(db, oid, VisitRecords.listGmosNorthVisits)
            case Instrument.GmosSouth => recordedSteps(db, oid, VisitRecords.listGmosSouthVisits)
            case _                    => List.empty[StepRecord.Output[_]]
          }).toList.flatten
        }

      def selectVisitsPageForObservation[S, D](
        oid:   Observation.Id,
        recs:  VisitRecords => List[(Visit.Id, VisitRecord[S, D])],
        count: Option[Int],
        after: Option[Visit.Id] = None
      ): F[ResultPage[VisitRecord.Output[S, D]]] =

        databaseRef.get.map { db =>

          val visits    = Database.visitRecordsAt(oid).get(db).toList.flatMap(recs)
          val events    = sortedEvents(db)(_.observationId === oid)
          val stepRecs  = visits.flatMap { case (vid, vr) => vr.steps.toList.tupleLeft(vid) }
          val soMap     = addEventsToSteps(oid, events, stepRecs, db.datasets).fproductLeft(_.stepId).toMap

          val seqEvents = events.collect {
            case e@SequenceEvent(_,_,_,_,_) => e
          }

          val allVisits = visits.map { case (vid, vr) =>
            VisitRecord.Output[S,D](
              oid,
              vid,
              vr.created,
              vr.static,
              vr.steps.keys.toList.flatMap { sid =>
                soMap.get(sid).toList
              }.sortBy(so => so.startTime.getOrElse(so.created)),
              seqEvents
            )
          }

          ResultPage.fromSeq(
            allVisits,
            count,
            after,
            _.visitId
          )
        }


      override def selectStepsPageForObservation[S, D](
        oid:    Observation.Id,
        visits: VisitRecords => List[(Visit.Id, VisitRecord[S, D])],
        count:  Option[Int],
        after:  Option[Step.Id] = None
      ): F[ResultPage[StepRecord.Output[D]]] =

        selectStepsForObservation(oid, visits).map { steps =>
          ResultPage.fromSeq(
            steps,
            count,
            after,
            _.stepId
          )
        }


      override def selectEventsPageForObservation(
        oid:      Observation.Id,
        count:    Option[Int],
        afterGid: Option[ExecutionEvent.Id]
      ): F[ResultPage[ExecutionEventModel]] =
        databaseRef.get.map { db =>
          ResultPage.fromSeq(sortedEvents(db)(_.observationId === oid), count, afterGid, _.id)
        }

      override def selectStepEventsPageForObservation(
        oid:      Observation.Id,
        count:    Option[Int],
        afterGid: Option[ExecutionEvent.Id] = None
      ): F[ResultPage[StepEvent]] =
        databaseRef.get.map { db =>
          val events = sortedEvents(db)(_.observationId === oid).collect {
            case s @ StepEvent(_, _, _, _, _, _, _) => s
          }
          ResultPage.fromSeq(events, count, afterGid, _.id)
        }


      override def selectDatasetEventsPageForObservation(
        oid:      Observation.Id,
        count:    Option[Int],
        afterGid: Option[ExecutionEvent.Id] = None
      ): F[ResultPage[DatasetEvent]] =
        databaseRef.get.map { db =>
          val events = sortedEvents(db)(_.observationId === oid).collect {
            case d @ DatasetEvent(_, _, _, _, _) => d
          }
          ResultPage.fromSeq(events, count, afterGid, _.id)
        }

      private def received: F[Instant] =
        Clock[F].realTime.map(d => Instant.ofEpochMilli(d.toMillis))

      override def insertVisit[SI, S, D](
        visitId:       Visit.Id,
        visit:         VisitRecord.Input[SI],
        prism:         Prism[VisitRecords, ListMap[Visit.Id, VisitRecord[S, D]]]
      )(implicit ev: InputValidator[SI, S]): F[VisitRecord.Output[S, D]] = {

        def record(in: ValidatedInput[VisitRecord[S, D]]): StateT[EitherInput, Database, VisitRecord[S, D]] =
          for {
            vr <- StateT.liftF(in.toEither)
            _  <- Database.visitRecordsAt(visit.observationId).mod_ { ovr =>
              prism.reverseGet(ovr.fold(ListMap(visitId -> vr)) { vrs =>
                prism.getOption(vrs).fold(ListMap(visitId -> vr))(_.updated(visitId, vr))
              }).some
            }
          } yield vr

        for {
          i <- Sync[F].delay(Instant.now)
          v  = visit.create[S, D](i)
          r <- databaseRef.modifyState(record(v).flipF).flatMap(_.liftTo[F])
        } yield VisitRecord.Output.init[S, D](visit.observationId, visitId, i, r.static)
      }

      override def insertStep[DI, S, D](
        stepId: Step.Id,
        step:   StepRecord.Input[DI],
        prism:  Prism[VisitRecords, ListMap[Visit.Id, VisitRecord[S, D]]]
      )(implicit ev: InputValidator[DI, D]): F[StepRecord.Output[D]] = {

        def record(in: ValidatedInput[StepRecord[D]]): StateT[EitherInput, Database, StepRecord[D]] =
          for {
            sr  <- StateT.liftF(in.toEither)
            _   <- Database.visitRecordAt(step.observationId, step.visitId, prism)
                    .andThen(VisitRecord.steps[S, D].asOptional)
                    .mod_(_.updated(stepId, sr))
          } yield sr

        for {
          i <- Sync[F].delay(Instant.now)
          s  = step.create[D](i)
          r <- databaseRef.modifyState(record(s).flipF).flatMap(_.liftTo[F])
        } yield StepRecord.Output.init[D](step.observationId, step.visitId, stepId, i, r.stepConfig)

      }

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

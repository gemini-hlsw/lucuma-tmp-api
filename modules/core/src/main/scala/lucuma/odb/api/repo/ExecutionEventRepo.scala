// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.data.StateT
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all._
import cats.effect.{Clock, Ref, Sync}
import eu.timepit.refined.types.all.NonNegInt
import lucuma.core.enums.Instrument
import lucuma.odb.api.model.{Database, DatasetModel, DatasetTable, EitherInput, ExecutionEventModel, InputValidator, Step, StepRecord, ValidatedInput, Visit, VisitRecord, VisitRecords, WhereExecutionEventInput}
import lucuma.odb.api.model.ExecutionEventModel.{DatasetEvent, SequenceEvent, StepEvent}
import lucuma.odb.api.model.syntax.databasestate._
import lucuma.odb.api.model.syntax.eitherinput._
import lucuma.odb.api.model.syntax.lens._
import lucuma.odb.api.model.syntax.optional._
import lucuma.core.model.{ExecutionEvent, Observation}
import lucuma.odb.api.model.query.SizeLimitedResult
import monocle.Prism

import java.time.Instant

import scala.collection.immutable.ListMap

sealed trait ExecutionEventRepo[F[_]] {

  def selectEvent(
    eid: ExecutionEvent.Id
  ): F[Option[ExecutionEventModel]]

  def selectWhere(
    where:  WhereExecutionEventInput,
    offset: Option[ExecutionEvent.Id],
    limit:  Option[NonNegInt]
  ): F[SizeLimitedResult[ExecutionEventModel]]

  def selectStepForId[S, D](
    oid:    Observation.Id,
    stepId: Step.Id,
    visits: VisitRecords => List[(Visit.Id, VisitRecord[S, D])]
  ): F[Option[StepRecord.Output[D]]]

  def selectVisitsForObservation[S, D](
    oid:   Observation.Id,
    recs:  VisitRecords => List[(Visit.Id, VisitRecord[S, D])]
  ): F[List[VisitRecord.Output[S, D]]]

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
  ): F[SequenceEvent.Result]

  def insertStepEvent(
    event: StepEvent.Add
  ): F[StepEvent.Result]

  def insertDatasetEvent(
    event: DatasetEvent.Add
  ): F[DatasetEvent.Result]

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

      override def selectWhere(
        where:  WhereExecutionEventInput,
        offset: Option[ExecutionEvent.Id],
        limit:  Option[NonNegInt]
      ): F[SizeLimitedResult[ExecutionEventModel]] =

        databaseRef.get.map { tables =>

          val all     = tables.executionEvents.rows
          val off     = offset.fold(all.iterator)(all.iteratorFrom).to(LazyList).map(_._2)
          val matches = off.filter(where.matches)
          SizeLimitedResult.Select.fromAll(matches.toList, limit)
        }

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
            case s: StepEvent     => m.updatedWith(s.location.stepId)(_.map(x => StepRecord.Output.stepEvents.modify(_ :+ s)(x)))
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
          case SequenceEvent(_, _, _, SequenceEvent.Location(o), _) => o === oid
          case StepEvent(_, _, _, StepEvent.Location(o, s), _)      => o === oid && s === stepId
          case DatasetEvent(_, _, _, DatasetModel.Id(o, s, _), _)   => o === oid && s === stepId
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
          db.observations.rows.get(oid).flatMap(_.manualConfig).map(_.instrument match {
            // How do you do this without breaking out the various cases?
            case Instrument.GmosNorth => recordedSteps(db, oid, VisitRecords.listGmosNorthVisits)
            case Instrument.GmosSouth => recordedSteps(db, oid, VisitRecords.listGmosSouthVisits)
            case _                    => List.empty[StepRecord.Output[_]]
          }).toList.flatten
        }

      override def selectVisitsForObservation[S, D](
        oid:   Observation.Id,
        recs:  VisitRecords => List[(Visit.Id, VisitRecord[S, D])]
      ): F[List[VisitRecord.Output[S, D]]] =

        databaseRef.get.map { db =>

          val visits    = Database.visitRecordsAt(oid).get(db).toList.flatMap(recs)
          val events    = sortedEvents(db)(_.observationId === oid)
          val stepRecs  = visits.flatMap { case (vid, vr) => vr.steps.toList.tupleLeft(vid) }
          val soMap     = addEventsToSteps(oid, events, stepRecs, db.datasets).fproductLeft(_.stepId).toMap

          val seqEvents = events.collect {
            case e@SequenceEvent(_,_,_,_,_) => e
          }

          visits.map { case (vid, vr) =>
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

      private def insertEvent[A, B](
        f: Instant => StateT[EitherInput, Database, A],
        r: A => B
      ): F[B] =
        for {
          w <- received
          e <- databaseRef.modifyState(f(w).flipF).flatMap(_.liftTo[F])
        } yield r(e)

      override def insertSequenceEvent(
        event: SequenceEvent.Add
      ): F[SequenceEvent.Result] =
        insertEvent(event.add, SequenceEvent.Result.apply)

      override def insertStepEvent(
        event: StepEvent.Add
      ): F[StepEvent.Result] =
        insertEvent(event.add, StepEvent.Result.apply)

      override def insertDatasetEvent(
        event: DatasetEvent.Add
      ): F[DatasetEvent.Result] =
        insertEvent(event.add, DatasetEvent.Result.apply)

    }


}

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.data.{State, StateT}
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all._
import cats.effect.{Clock, Ref, Sync}
import eu.timepit.refined.cats._
import eu.timepit.refined.types.all.PosInt
import lucuma.core.`enum`.Instrument
import lucuma.odb.api.model.{AtomModel, Database, DatasetModel, EitherInput, ExecutionEventModel, InputValidator, InstrumentConfigModel, Sequence, SequenceModel, StepConfig, Step, StepModel, StepRecord, ValidatedInput, Visit, VisitRecord, VisitRecords}
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

  /** Page datasets associated with an observation  */
  def selectDatasetsPageForObservation(
    oid:   Observation.Id,
    count: Option[Int],
    after: Option[(Step.Id, PosInt)] = None
  ): F[ResultPage[DatasetModel]]

  /** Page datasets associated with an individual step. */
  def selectDatasetsPageForStep(
    stepId: Step.Id,
    count: Option[Int],
    after: Option[PosInt] = None
  ): F[ResultPage[DatasetModel]]

  def selectStepForId[S, D](
    oid:    Observation.Id,
    stepId: Step.Id,
    recs:   VisitRecords => Option[ListMap[Visit.Id, VisitRecord[S, D]]]
  ): F[Option[StepRecord.Output[D]]]

  /** Page executed visits associated with an observation. */
  def selectVisitsPageForObservation[S, D](
    oid:   Observation.Id,
    recs:  VisitRecords => Option[ListMap[Visit.Id, VisitRecord[S, D]]],
    count: Option[Int],
    after: Option[Visit.Id] = None
  ): F[ResultPage[VisitRecord.Output[S, D]]]

  /** Page executed steps associated with an observation. */
  def selectStepsPageForObservation[S, D](
    oid:   Observation.Id,
    recs:  VisitRecords => Option[ListMap[Visit.Id, VisitRecord[S, D]]],
    count: Option[Int],
    after: Option[Step.Id] = None
  ): F[ResultPage[StepRecord.Output[D]]]

  /** Select all recorded steps for an observation at once. */
  def selectStepsForObservation[S, D](
    oid:  Observation.Id,
    recs: VisitRecords => Option[ListMap[Visit.Id, VisitRecord[S, D]]]
  ): F[List[StepRecord.Output[D]]]

  def selectExistentialStepsForObservation(
    oid:  Observation.Id,
   ): F[List[StepRecord.Output[_]]]

  def selectRemainingAtoms[S, D](
    oid:  Observation.Id,
    recs: VisitRecords => Option[ListMap[Visit.Id, VisitRecord[S, D]]],
    seq:  InstrumentConfigModel => Option[Sequence[D]]
  ): F[Sequence[D]]

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
        recs:   List[(Visit.Id, (Step.Id, StepRecord[D]))]
      ): List[StepRecord.Output[D]] = {

        val stepMap = recs.map { case (vid, (sid, r)) =>
          sid -> StepRecord.Output.init(oid, vid, sid, r.created, r.stepConfig)
        }.toMap

        // Steps and their associated dataset and step events.
        val byStep = events.foldRight(stepMap) { (e, m) =>
          e match {
            case _: SequenceEvent => m
            case d: DatasetEvent  => m.updatedWith(d.stepId)(_.map(x => StepRecord.Output.datasetEvents.modify(_ :+ d)(x)))
            case s: StepEvent     => m.updatedWith(s.stepId)(_.map(x => StepRecord.Output.stepEvents.modify(_ :+ s)(x)))
          }
        }

        // Create executed steps from the event information.
        byStep.values.toList.sortBy(s => (s.startTime, s.stepId))
      }

      private def recordedStep[S, D](
        db:     Database,
        oid:    Observation.Id,
        stepId: Step.Id,
        recs:   VisitRecords => Option[ListMap[Visit.Id, VisitRecord[S, D]]]
      ): Option[StepRecord.Output[D]] = {

        val events  = sortedEvents(db) {
          case SequenceEvent(_, o, _, _, _)         => o === oid
          case StepEvent(_, o, _, s, _, _, _)       => o === oid && s === stepId
          case DatasetEvent(_, o, _, s, _, _, _, _) => o === oid && s === stepId
        }

        val stepRec = Database.visitRecordsAt(oid).get(db).toList.flatMap { vrs =>
          recs(vrs).toList.flatMap(_.toList).flatMap { case (vid, vr) =>
            vr.steps.get(stepId).tupleLeft(stepId).toList.tupleLeft(vid)
          }
        }

        addEventsToSteps(oid, events, stepRec).headOption

      }

      override def selectStepForId[S, D](
        oid:    Observation.Id,
        stepId: Step.Id,
        recs:   VisitRecords => Option[ListMap[Visit.Id, VisitRecord[S, D]]]
      ): F[Option[StepRecord.Output[D]]] =
        databaseRef.get.map(recordedStep(_, oid, stepId, recs))

      private def recordedSteps[S, D](
        db:   Database,
        oid:  Observation.Id,
        recs: Option[ListMap[Visit.Id, VisitRecord[S, D]]]
      ): List[StepRecord.Output[D]] = {

        val events   = sortedEvents(db)(_.observationId === oid)
        val stepRecs = recs.toList.flatMap(_.toList).flatMap { case (vid, vr) => vr.steps.toList.tupleLeft(vid) }
        addEventsToSteps(oid, events, stepRecs)
      }

      private def recordedSteps[S, D](
        db:   Database,
        oid:  Observation.Id,
        recs: VisitRecords => Option[ListMap[Visit.Id, VisitRecord[S, D]]]
      ): List[StepRecord.Output[D]] =
        recordedSteps[S, D](db, oid, Database.visitRecordsAt(oid).get(db).flatMap(recs))

      override def selectStepsForObservation[S, D](
        oid:  Observation.Id,
        recs: VisitRecords => Option[ListMap[Visit.Id, VisitRecord[S, D]]]
      ): F[List[StepRecord.Output[D]]] =
        databaseRef.get.map(recordedSteps(_, oid, recs))

      override def selectExistentialStepsForObservation(
        oid:  Observation.Id,
      ): F[List[StepRecord.Output[_]]] =
        databaseRef.get.map { db =>
          db.observations.rows.get(oid).flatMap(_.config).map(_.instrument match {
            // How do you do this without breaking out the various cases?
            case Instrument.GmosNorth => recordedSteps(db, oid, VisitRecords.gmosNorthVisits.getOption _)
            case Instrument.GmosSouth => recordedSteps(db, oid, VisitRecords.gmosSouthVisits.getOption _)
            case _                    => List.empty[StepRecord.Output[_]]
          }).toList.flatten
        }

      def selectVisitsPageForObservation[S, D](
        oid:   Observation.Id,
        recs:  VisitRecords => Option[ListMap[Visit.Id, VisitRecord[S, D]]],
        count: Option[Int],
        after: Option[Visit.Id] = None
      ): F[ResultPage[VisitRecord.Output[S, D]]] =

        databaseRef.get.map { db =>

          val vMap      = Database.visitRecordsAt(oid).get(db).flatMap(recs).getOrElse(ListMap.empty)
          val events    = sortedEvents(db)(_.observationId === oid)
          val stepRecs  = vMap.toList.flatMap { case (vid, vr) => vr.steps.toList.tupleLeft(vid) }
          val soMap     = addEventsToSteps(oid, events, stepRecs).fproductLeft(_.stepId).toMap

          val seqEvents = events.collect {
            case e@SequenceEvent(_,_,_,_,_) => e
          }

          val allVisits = vMap.toList.map { case (vid, vr) =>
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
        oid:   Observation.Id,
        recs:  VisitRecords => Option[ListMap[Visit.Id, VisitRecord[S, D]]],
        count: Option[Int],
        after: Option[Step.Id] = None
      ): F[ResultPage[StepRecord.Output[D]]] =

        selectStepsForObservation(oid, recs).map { steps =>
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
            case d @ DatasetEvent(_, _, _, _, _, _, _, _) => d
          }
          ResultPage.fromSeq(events, count, afterGid, _.id)
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
        stepId: Step.Id,
        count:  Option[Int],
        after:  Option[PosInt] = None
      ): F[ResultPage[DatasetModel]] =

        databaseRef.get.map { db =>
          db.executionEvents.rows.values.collect {
            case de: DatasetEvent if de.stepId === stepId => de.toDataset
          }.toList.flattenOption.distinct.sortBy(_.index)
        }.map { all =>

          ResultPage.fromSeq(
            all,
            count,
            after,
            _.index
          )

        }


      private def remainingAtoms[S, D](
        db:   Database,
        oid:  Observation.Id,
        recs: VisitRecords => Option[ListMap[Visit.Id, VisitRecord[S, D]]],
        seq:  InstrumentConfigModel => Option[Sequence[D]]
      ): Sequence[D] = {

        val recorded: List[(StepConfig[D], Boolean)] =
          recordedSteps(db, oid, recs).map(r => (r.stepConfig, r.isExecuted))

        val wholeSequence: List[AtomModel[StepModel[D]]] =
          db.observations.rows.get(oid).flatMap(_.config).flatMap(seq).toList.flatMap(_.atoms)

        // We remove atoms from the wholeSequence when a matching, contiguous,
        // executed series of steps is found in the recorded list.  This is
        // still too simplistic since "contiguous" could be separated by days
        // of time. TBD.

        // Also, we need to filter out any steps that were taken under different
        // static configurations.

        def isExecuted(a: AtomModel[StepModel[D]]): State[List[(StepConfig[D], Boolean)], Boolean] = {
          val steps = a.steps.tupleRight(true).toList

          for {
            rs <- State.get[List[(StepConfig[D], Boolean)]]
            i   = rs.indexOfSlice(steps, 0)
            _  <- State.set[List[(StepConfig[D], Boolean)]] {
              if (i < 0) rs else rs.patch(i, Nil, steps.length)
            }
          } yield i > -1
        }

        SequenceModel(
          // Filter the sequence, removing any atoms we consider executed.
          wholeSequence
            .zip(wholeSequence.traverse(isExecuted).runA(recorded).value)
            .filterNot(_._2)
            .map(_._1)
        )
      }

      override def selectRemainingAtoms[S, D](
        oid:  Observation.Id,
        recs: VisitRecords => Option[ListMap[Visit.Id, VisitRecord[S, D]]],
        seq:  InstrumentConfigModel => Option[Sequence[D]]
      ): F[Sequence[D]] =
        databaseRef.get.map(remainingAtoms(_, oid, recs, seq))

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

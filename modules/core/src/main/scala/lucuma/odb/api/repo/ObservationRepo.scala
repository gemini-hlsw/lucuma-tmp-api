// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.core.model.{Asterism, Observation, Program, Target}
import lucuma.odb.api.model.{AsterismModel, ConstraintSetModel, ScienceRequirementsModel, Event, InputError, InstrumentConfigModel, ObservationModel, PlannedTimeSummaryModel, TargetModel, ValidatedInput}
import lucuma.odb.api.model.AsterismModel.AsterismEvent
import lucuma.odb.api.model.ObservationModel.ObservationEvent
import lucuma.odb.api.model.TargetModel.TargetEvent
import lucuma.odb.api.model.syntax.validatedinput._
import lucuma.core.optics.state.all._
import cats.Eq
import cats.data.{EitherT, State}
import cats.effect.{Async, Ref}
import cats.implicits._
import clue.data.{Assign, Ignore, Input, Unassign}

sealed trait ObservationRepo[F[_]] extends TopLevelRepo[F, Observation.Id, ObservationModel] {

  def selectPageForAsterism(
    aid:            Asterism.Id,
    pid:            Option[Program.Id]     = None,
    count:          Option[Int]            = None,
    afterGid:       Option[Observation.Id] = None,
    includeDeleted: Boolean                = false
  ): F[ResultPage[ObservationModel]]

  def selectPageForObservations(
    oids:           Set[Observation.Id],
    count:          Option[Int]            = None,
    afterGid:       Option[Observation.Id] = None,
    includeDeleted: Boolean                = false
  ): F[ResultPage[ObservationModel]]

  def selectPageForProgram(
    pid:            Program.Id,
    count:          Option[Int]            = None,
    afterGid:       Option[Observation.Id] = None,
    includeDeleted: Boolean                = false
  ): F[ResultPage[ObservationModel]]

  def selectPageForTarget(
    tid:            Target.Id,
    pid:            Option[Program.Id]     = None,
    count:          Option[Int]            = None,
    afterGid:       Option[Observation.Id] = None,
    includeDeleted: Boolean                = false
  ): F[ResultPage[ObservationModel]]

  def selectManualConfig(
    oid: Observation.Id
  ): F[Option[InstrumentConfigModel]]

  def insert(input: ObservationModel.Create): F[ObservationModel]

  def edit(edit: ObservationModel.Edit): F[ObservationModel]

  def editPointing(edit: ObservationModel.EditPointing): F[List[ObservationModel]]

  def groupByConstraintSet(pid: Program.Id): F[List[ObservationModel.Group[ConstraintSetModel]]]

  def bulkEditConstraintSet(edit: ConstraintSetModel.BulkEdit): F[List[ObservationModel]]

  def bulkEditScienceRequirements(edit: ScienceRequirementsModel.BulkEdit): F[List[ObservationModel]]

}

object ObservationRepo {

  def create[F[_]: Async](
    tablesRef:    Ref[F, Tables],
    eventService: EventService[F]
  ): ObservationRepo[F] =

    new TopLevelRepoBase[F, Observation.Id, ObservationModel](
      tablesRef,
      eventService,
      Tables.lastObservationId,
      Tables.observations,
      (editType, model) => ObservationEvent(_, editType, model)
    ) with ObservationRepo[F]
      with LookupSupport {

      override def selectPageForAsterism(
        aid:            Asterism.Id,
        pid:            Option[Program.Id],
        count:          Option[Int],
        afterGid:       Option[Observation.Id],
        includeDeleted: Boolean
      ): F[ResultPage[ObservationModel]] =

        selectPageFiltered(count, afterGid, includeDeleted) { obs =>
          obs.pointing.contains(aid.asLeft[Target.Id]) && pid.forall(_ === obs.programId)
        }

      override def selectPageForObservations(
        oids:           Set[Observation.Id],
        count:          Option[Int],
        afterGid:       Option[Observation.Id],
        includeDeleted: Boolean
      ): F[ResultPage[ObservationModel]] =

        selectPageFiltered(count, afterGid, includeDeleted) { o => oids(o.id) }

      override def selectPageForProgram(
        pid:            Program.Id,
        count:          Option[Int],
        afterGid:       Option[Observation.Id],
        includeDeleted: Boolean
      ): F[ResultPage[ObservationModel]] =

        selectPageFiltered(count, afterGid, includeDeleted) { _.programId === pid }

      override def selectPageForTarget(
        tid:            Target.Id,
        pid:            Option[Program.Id],
        count:          Option[Int],
        afterGid:       Option[Observation.Id],
        includeDeleted: Boolean
      ): F[ResultPage[ObservationModel]] =

        selectPageFiltered(count, afterGid, includeDeleted) { obs =>
          // this includes only observations that directly reference a target,
          // but not those referencing an asterism that references the target
          obs.pointing.contains(tid.asRight[Asterism.Id]) && pid.forall(_ === obs.programId)
        }

      override def selectManualConfig(
        oid: Observation.Id
      ): F[Option[InstrumentConfigModel]] =
        tablesRef.get.map { tables =>
          for {
            o <- tables.observations.get(oid)
            c <- o.config
            m <- c.dereference[State[Tables, *], Tables](TableState).runA(tables).value
          } yield m
        }

      override def insert(newObs: ObservationModel.Create): F[ObservationModel] = {

        // Create the observation, keeping track of the asterism or target (if
        // either) and constraint (if any) that will be linked.
        def create(s: PlannedTimeSummaryModel): F[(Option[AsterismModel], Option[TargetModel], ObservationModel)] =
          EitherT(
            tablesRef.modify { tables =>
              val (tablesʹ, o) = newObs.create[State[Tables, *], Tables](TableState, s).run(tables).value

              o.map { obs => (
                obs.asterismId.flatMap(tables.asterisms.get),
                obs.targetId.flatMap(tables.targets.get),
                obs
              )}.fold(
                err => (tables,  InputError.Exception(err).asLeft),
                tup => (tablesʹ, tup.asRight)
              )

            }
          ).rethrowT

        for {
          s   <- PlannedTimeSummaryModel.random[F]
          tup <- create(s)
          (oasterism, otarget, obs) = tup
          _   <- oasterism.traverse_(a => eventService.publish(AsterismEvent(_, Event.EditType.Updated, a)))
          _   <- otarget.traverse_(t => eventService.publish(TargetEvent(_, Event.EditType.Updated, t)))
          _   <- eventService.publish(ObservationEvent(_, Event.EditType.Created, obs))
        } yield obs

      }

      type Pointing = Option[Either[Asterism.Id, Target.Id]]
      private val noPointing: Pointing = Option.empty

      private def updatePointing(
        oids:          List[Observation.Id],
        asterism:      Input[Asterism.Id],
        target:        Input[Target.Id],
      ): State[Tables, ValidatedInput[(List[ObservationModel], List[AsterismModel], List[TargetModel])]] = {

        val updateAsterism: Pointing => Pointing = in =>
          asterism match {
            case Ignore      => in
            case Unassign    => in.flatMap(_.fold(_ => noPointing, _ => in))
            case Assign(aid) => aid.asLeft[Target.Id].some
          }

        val updateTarget: Pointing => Pointing = in =>
          target match {
            case Ignore      => in
            case Unassign    => in.flatMap(_.fold(_ => in, _ => noPointing))
            case Assign(tid) => tid.asRight[Asterism.Id].some
          }

        val updatePointing = updateAsterism andThen updateTarget

        // Don't send a change event for the old id if the id Input is Ignore or the value is unchanged.
        // Also don't send an event for the new id (if Assign) if it is always the same as the old id.
        def ids4Event[A: Eq](oa: Option[A], ia: Input[A]): List[A] = (oa, ia) match {
          case (_, Ignore)                => Nil
          case (Some(a), Unassign)        => List(a)
          case (None, Unassign)           => Nil
          case (Some(aOld), Assign(aNew)) => if (aOld === aNew) Nil else List(aOld, aNew)
          case (None, Assign(aNew))       => List(aNew)
        }

        for {
          vos <- TableState.observation.lookupAllValidated[State[Tables, *]](oids)
          va  <- asterism.toOption.traverse(TableState.asterism.lookupValidated[State[Tables, *]]).map(_.sequence)
          vt  <- target.toOption.traverse(TableState.target.lookupValidated[State[Tables, *]]).map(_.sequence)
          tb  <- State.get
        } yield (vos, va, vt).mapN { (os, _, _) =>

          val (updatedOs, updatedAids, updatedTids) =
            os.foldLeft((List.empty[ObservationModel], Set.empty[Asterism.Id], Set.empty[Target.Id])) {
              case ((osʹ, aids, tids), o) =>
                val newO = ObservationModel.pointing.modify(updatePointing)(o)
                (
                  newO  :: osʹ,
                  aids  ++ ids4Event(ObservationModel.asterism.getOption(o), asterism),
                  tids  ++ ids4Event(ObservationModel.target.getOption(o), target),
                )
            }

          (
           updatedOs,
           updatedAids.toList.map(tb.asterisms.apply),
           updatedTids.toList.map(tb.targets.apply)
          )
        }
      }

      private def doEdit(
        oids:          List[Observation.Id],
        editor:        State[ObservationModel, Unit],
        asterism:      Input[Asterism.Id],
        target:        Input[Target.Id]
      ): F[List[ObservationModel]] = {

        val update: State[Tables, ValidatedInput[(List[ObservationModel], List[AsterismModel], List[TargetModel])]] =
          for {
            voat  <- updatePointing(oids, asterism, target)
            voatʹ <- voat.map { case (os, as, ts) =>
              val os2 = os.map(o => editor.runS(o).value)
              Tables.observations.mod(_ ++ os2.map(o => o.id -> o)).as((os2, as, ts))
            }.sequence
          } yield voatʹ

        for {
          oat <- tablesRef.modifyState(update).flatMap(_.liftTo[F])
          (os, as, ts) = oat
          _    <- os.traverse_(o => eventService.publish(ObservationModel.ObservationEvent.updated(o)))
          _    <- as.traverse_(a => eventService.publish(AsterismModel.AsterismEvent.updated(a)))
          _    <- ts.traverse_(t => eventService.publish(TargetModel.TargetEvent.updated(t)))
        } yield os

      }

      override def editPointing(
        edit: ObservationModel.EditPointing
      ): F[List[ObservationModel]] =

        for {
          e  <- edit.pointing.liftTo[F]
          os <- doEdit(
              edit.observationIds,
              State.pure[ObservationModel, Unit](()),
              e.fold(Input.unassign[Asterism.Id])(_.fold(aid => Input(aid), _ => Input.ignore[Asterism.Id])),
              e.fold(Input.unassign[Target.Id])(_.fold(_ => Input.ignore[Target.Id], tid => Input(tid)))
            )
        } yield os

      override def edit(
        edit: ObservationModel.Edit
      ): F[ObservationModel] =

        (edit.editor, edit.pointing).mapN { case (e, (a, t)) =>
          doEdit(List(edit.observationId), e, a, t)
        }.liftTo[F].flatten.map(_.head)


      override def groupByConstraintSet(
        pid: Program.Id
      ): F[List[ObservationModel.Group[ConstraintSetModel]]] =
        tablesRef.get.map { t =>
          t.observations
           .filter { case (_, o) => o.programId === pid }
           .groupBy { case (_, o) => o.constraintSet }
           .view
           .mapValues(_.keySet)
           .toList
           .sortBy(_._2.head)
           .map { case (c, oids) => ObservationModel.Group(c, oids) }
        }

      override def bulkEditConstraintSet(
        edit: ConstraintSetModel.BulkEdit
      ): F[List[ObservationModel]] = {

        val update =
          for {
            vos <- TableState.observation.lookupAllValidated[State[Tables, *]](edit.observationIds)
            os  <- (vos, edit.constraintSet.editor).mapN { (os, ed) =>
                     val os2 = os.map {
                       ObservationModel.constraintSet.modify { cs => ed.runS(cs).value }
                     }
                     Tables.observations.mod(_ ++ os2.map(o => o.id -> o)).as(os2)
                   }.sequence
          } yield os

        for {
          os <- tablesRef.modifyState(update).flatMap(_.liftTo[F])
          _  <- os.traverse_(o => eventService.publish(ObservationModel.ObservationEvent.updated(o)))
        } yield os

      }

      override def bulkEditScienceRequirements(edit: ScienceRequirementsModel.BulkEdit): F[List[ObservationModel]] = {
        val update =
          for {
            vos <- TableState.observation.lookupAllValidated[State[Tables, *]](edit.observationIds)
            os  <- (vos, edit.scienceRequirements.editor).mapN { (os, ed) =>
                     val os2 = os.map {
                       ObservationModel.scienceRequirements.modify { cs => ed.runS(cs).value }
                     }
                     Tables.observations.mod(_ ++ os2.map(o => o.id -> o)).as(os2)
                   }.sequence
          } yield os

        for {
          os <- tablesRef.modifyState(update).flatMap(_.liftTo[F])
          _  <- os.traverse_(o => eventService.publish(ObservationModel.ObservationEvent.updated(o)))
        } yield os
      }

    }
}

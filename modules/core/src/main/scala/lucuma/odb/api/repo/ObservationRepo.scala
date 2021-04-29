// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.core.model.{Asterism, ConstraintSet, Observation, Program, Target}
import lucuma.odb.api.model.{AsterismModel, ConstraintSetModel, Event, InputError, ObservationModel, PlannedTimeSummaryModel, TargetModel, ValidatedInput}
import lucuma.odb.api.model.AsterismModel.AsterismEvent
import lucuma.odb.api.model.ObservationModel.ObservationEvent
import lucuma.odb.api.model.TargetModel.TargetEvent
import lucuma.odb.api.model.syntax.validatedinput._
import cats.Eq
import cats.data.{EitherT, State}
import cats.effect.Sync
import cats.implicits._
import clue.data.{Assign, Ignore, Input, Unassign}
import monocle.state.all._
import cats.effect.Ref

sealed trait ObservationRepo[F[_]] extends TopLevelRepo[F, Observation.Id, ObservationModel] {

  def selectPageForAsterism(
    aid:            Asterism.Id,
    pid:            Option[Program.Id]     = None,
    count:          Int                    = Integer.MAX_VALUE,
    afterGid:       Option[Observation.Id] = None,
    includeDeleted: Boolean                = false
  ): F[ResultPage[ObservationModel]]

  def selectPageForConstraintSet(
    csid:           ConstraintSet.Id,
    count:          Int                    = Integer.MAX_VALUE,
    afterGid:       Option[Observation.Id] = None,
    includeDeleted: Boolean                = false
  ): F[ResultPage[ObservationModel]]

  def selectPageForProgram(
    pid:            Program.Id,
    count:          Int                    = Integer.MAX_VALUE,
    afterGid:       Option[Observation.Id] = None,
    includeDeleted: Boolean                = false
  ): F[ResultPage[ObservationModel]]

  def selectPageForTarget(
    tid:            Target.Id,
    pid:            Option[Program.Id]     = None,
    count:          Int                    = Integer.MAX_VALUE,
    afterGid:       Option[Observation.Id] = None,
    includeDeleted: Boolean                = false
  ): F[ResultPage[ObservationModel]]

  def insert(input: ObservationModel.Create): F[ObservationModel]

  def edit(edit: ObservationModel.Edit): F[ObservationModel]

  def editPointing(edit: ObservationModel.EditPointing): F[List[ObservationModel]]

  def editConstraintSet(edit: ObservationModel.EditConstraintSet): F[List[ObservationModel]]

}

object ObservationRepo {

  def create[F[_]: Sync](
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
        count:          Int,
        afterGid:       Option[Observation.Id],
        includeDeleted: Boolean
      ): F[ResultPage[ObservationModel]] =

        selectPageFiltered(count, afterGid, includeDeleted) { obs =>
          obs.pointing.contains(aid.asLeft[Target.Id]) && pid.forall(_ === obs.programId)
        }

      override def selectPageForConstraintSet(
        csid:           ConstraintSet.Id,
        count:          Int,
        afterGid:       Option[Observation.Id],
        includeDeleted: Boolean
      ): F[ResultPage[ObservationModel]] =

        selectPageFiltered(count, afterGid, includeDeleted) { _.constraintSetId.exists(_ === csid) }

      override def selectPageForProgram(
        pid:            Program.Id,
        count:          Int,
        afterGid:       Option[Observation.Id],
        includeDeleted: Boolean
      ): F[ResultPage[ObservationModel]] =

        selectPageFiltered(count, afterGid, includeDeleted) { _.programId === pid }

      override def selectPageForTarget(
        tid:            Target.Id,
        pid:            Option[Program.Id],
        count:          Int,
        afterGid:       Option[Observation.Id],
        includeDeleted: Boolean
      ): F[ResultPage[ObservationModel]] =

        selectPageFiltered(count, afterGid, includeDeleted) { obs =>
          // this includes only observations that directly reference a target,
          // but not those referencing an asterism that references the target
          obs.pointing.contains(tid.asRight[Asterism.Id]) && pid.forall(_ === obs.programId)
        }


      override def insert(newObs: ObservationModel.Create): F[ObservationModel] = {

        // Create the observation, keeping track of the asterism or target (if
        // either) and constraint (if any) that will be linked.
        def create(s: PlannedTimeSummaryModel): F[(Option[AsterismModel], Option[TargetModel], Option[ConstraintSetModel], ObservationModel)] =
          EitherT(
            tablesRef.modify { tables =>
              val (tablesʹ, o) = newObs.create(TableState, s).run(tables).value

              o.map { obs => (
                obs.asterismId.flatMap(tables.asterisms.get),
                obs.targetId.flatMap(tables.targets.get),
                obs.constraintSetId.flatMap(tables.constraintSets.get),
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
          (oasterism, otarget, oconstraint, obs) = tup
          _   <- oasterism.traverse_(a => eventService.publish(AsterismEvent(_, Event.EditType.Updated, a)))
          _   <- otarget.traverse_(t => eventService.publish(TargetEvent(_, Event.EditType.Updated, t)))
          _   <- oconstraint.traverse_(cs => eventService.publish(ConstraintSetModel.ConstraintSetEvent(_, Event.EditType.Updated, cs)))
          _   <- eventService.publish(ObservationEvent(_, Event.EditType.Created, obs))
        } yield obs

      }

      type Pointing = Option[Either[Asterism.Id, Target.Id]]
      private val noPointing: Pointing = Option.empty

      private def updatePointingAndConstraintSet(
        oids:          List[Observation.Id],
        asterism:      Input[Asterism.Id],
        target:        Input[Target.Id],
        constraintSet: Input[ConstraintSet.Id]
      ): State[Tables, ValidatedInput[(List[ObservationModel], List[AsterismModel], List[TargetModel], List[ConstraintSetModel])]] = {

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

        val updateConstraintSet: Option[ConstraintSet.Id] => Option[ConstraintSet.Id] = in =>
          constraintSet match {
            case Ignore        => in
            case Unassign      => None
            case Assign(csid) => csid.some
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
          vos <- oids.traverse(TableState.observation.lookup).map(_.sequence)
          va  <- asterism.toOption.traverse(TableState.asterism.lookup).map(_.sequence)
          vt  <- target.toOption.traverse(TableState.target.lookup).map(_.sequence)
          vcs <- constraintSet.toOption.traverse(TableState.constraintSet.lookup).map(_.sequence)
          tb  <- State.get
        } yield (vos, va, vt, vcs).mapN { (os, _, _, _) =>

          val (updatedOs, updatedAids, updatedTids, updatedCsids) =
            os.foldLeft((List.empty[ObservationModel], Set.empty[Asterism.Id], Set.empty[Target.Id], Set.empty[ConstraintSet.Id])) {
              case ((osʹ, aids, tids, csids), o) =>
                val oModP = ObservationModel.pointing.modify(updatePointing)(o)
                val newO = ObservationModel.constraintSet.modify(updateConstraintSet)(oModP)
                (
                  newO  :: osʹ,
                  aids  ++ ids4Event(ObservationModel.asterism.getOption(o), asterism),
                  tids  ++ ids4Event(ObservationModel.target.getOption(o), target),
                  csids ++ ids4Event(ObservationModel.constraintSet.get(o), constraintSet)
                )
            }

          (
           updatedOs,
           updatedAids.toList.map(tb.asterisms.apply),
           updatedTids.toList.map(tb.targets.apply),
           updatedCsids.toList.map(tb.constraintSets.apply)
          )
        }
      }

      private def doEdit(
        oids:          List[Observation.Id],
        editor:        State[ObservationModel, Unit],
        asterism:      Input[Asterism.Id],
        target:        Input[Target.Id],
        constraintSet: Input[ConstraintSet.Id]
      ): F[List[ObservationModel]] = {

        val update: State[Tables, ValidatedInput[(List[ObservationModel], List[AsterismModel], List[TargetModel], List[ConstraintSetModel])]] =
          for {
            voatc  <- updatePointingAndConstraintSet(oids, asterism, target, constraintSet)
            voatcʹ <- voatc.map { case (os, as, ts, cs) =>
              val os2 = os.map(o => editor.runS(o).value)
              Tables.observations.mod(_ ++ os2.map(o => o.id -> o)).as((os2, as, ts, cs))
            }.sequence
          } yield voatcʹ

        for {
          oatc <- tablesRef.modifyState(update).flatMap(_.liftTo[F])
          (os, as, ts, cs) = oatc
          _    <- os.traverse_(o => eventService.publish(ObservationModel.ObservationEvent.updated(o)))
          _    <- as.traverse_(a => eventService.publish(AsterismModel.AsterismEvent.updated(a)))
          _    <- ts.traverse_(t => eventService.publish(TargetModel.TargetEvent.updated(t)))
          _    <- cs.traverse_(c => eventService.publish(ConstraintSetModel.ConstraintSetEvent.updated(c)))
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
              e.fold(Input.unassign[Target.Id])(_.fold(_ => Input.ignore[Target.Id], tid => Input(tid))),
              Input.ignore[ConstraintSet.Id]
            )
        } yield os

      override def editConstraintSet(
        edit: ObservationModel.EditConstraintSet
      ): F[List[ObservationModel]] =
          doEdit(
            edit.observationIds,
            State.pure[ObservationModel, Unit](()),
            Input.ignore[Asterism.Id],
            Input.ignore[Target.Id],
            edit.constraintSetId.fold(Input.unassign[ConstraintSet.Id])(Input(_))
          )

      override def edit(
        edit: ObservationModel.Edit
      ): F[ObservationModel] =

        (edit.editor, edit.pointing).mapN { case (e, (a, t)) =>
          doEdit(List(edit.observationId), e, a, t, edit.constraintSetId)
        }.liftTo[F].flatten.map(_.head)

    }
}

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.core.model.{Asterism, ConstraintSet, Observation, Program, Target}
import lucuma.odb.api.model.{AsterismModel, ConstraintSetModel, Event, InputError, ObservationModel, PlannedTimeSummaryModel, TargetModel, ValidatedInput}
import lucuma.odb.api.model.AsterismModel.AsterismEvent
import lucuma.odb.api.model.ObservationModel.ObservationEvent
import lucuma.odb.api.model.TargetModel.TargetEvent
import lucuma.odb.api.model.syntax.validatedinput._

import cats.MonadError
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

  def shareWithConstraintSet(oid: Observation.Id, csid: ConstraintSet.Id): F[ObservationModel]

  def unshareWithConstraintSet(oid: Observation.Id, csid: ConstraintSet.Id): F[ObservationModel]

  def unsetConstraintSet(oid: Observation.Id)(implicit F: MonadError[F, Throwable]): F[ObservationModel]

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

        selectPageFromIds(count, afterGid, includeDeleted) { tables =>
          tables.constraintSetObservation.selectRight(csid)
        }

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
        // either) that will be linked.
        def create(s: PlannedTimeSummaryModel): F[(Option[AsterismModel], Option[TargetModel], ObservationModel)] =
          EitherT(
            tablesRef.modify { tables =>
              {
                tryNotFindObservation(tables, newObs.observationId)     *>
                tryFindProgram(tables, newObs.programId)                *>
                (newObs.asterismId.traverse(tryFindAsterism(tables, _)),
                 newObs.targetId.traverse(tryFindTarget(tables, _)),
                 newObs.withId(s)
                ).mapN { case (oa, ot, f) =>
                  createAndInsert(newObs.observationId, f).map{o => (oa, ot, o)}
                }
              }.fold(
                err => (tables, InputError.Exception(err).asLeft),
                _.run(tables).value.map(_.asRight)
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

      override def shareWithConstraintSet(
        oid:  Observation.Id,
        csid: ConstraintSet.Id
      ): F[ObservationModel] =
        shareWithOneUnique(
          "constraintSet",
          oid,
          csid,
          TableState.constraintSet,
          Tables.constraintSetObservation,
          ConstraintSetModel.ConstraintSetEvent.updated
        )

      override def unshareWithConstraintSet(
        oid:  Observation.Id,
        csid: ConstraintSet.Id
      ): F[ObservationModel] =
        unshareWithOneUnique(
          "constraintSet",
          oid,
          csid,
          TableState.constraintSet,
          Tables.constraintSetObservation,
          ConstraintSetModel.ConstraintSetEvent.updated
        )

      override def unsetConstraintSet(oid: Observation.Id)(implicit F: MonadError[F, Throwable]): F[ObservationModel] = {
        val doUpdate: F[(ObservationModel, Option[ConstraintSetModel])] =
          tablesRef.modify { oldTables =>
            val obs = focusOn(oid).get(oldTables).toValidNec(InputError.missingReference("id", oid.show))
            val cs  =
              obs.fold(
                _ => None,
                o => oldTables.constraintSetObservation.selectLeft(o.id).flatMap(csId => Tables.constraintSet(csId).get(oldTables))
              )
            val tables = cs.fold(oldTables)(_ => Tables.constraintSetObservation.modify(_.removeRight(oid))(oldTables))
            (tables, obs.tupleRight(cs))
          }.flatMap(_.liftTo[F])

          for {
            t         <- doUpdate
            (obs, cs)  = t
            _         <- cs.fold(F.unit)(c => eventService.publish(ConstraintSetModel.ConstraintSetEvent.updated(c)))
          } yield obs
      }

      type Pointing = Option[Either[Asterism.Id, Target.Id]]
      private val noPointing: Pointing = Option.empty

      private def updatePointing(
        oids:     List[Observation.Id],
        asterism: Input[Asterism.Id],
        target:   Input[Target.Id]
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

        for {
          vos <- oids.traverse(TableState.observation).map(_.sequence)
          va  <- asterism.toOption.traverse(TableState.asterism).map(_.sequence)
          vt  <- target.toOption.traverse(TableState.target).map(_.sequence)
          tb  <- State.get
        } yield (vos, va, vt).mapN { (os, a, t) =>

          val (updatedOs, updatedAids, updatedTids) =
            os.foldLeft((List.empty[ObservationModel], a.map(_.id).toSet, t.map(_.id).toSet)) {
              case ((osʹ, aids, tids), o) =>
                (
                  ObservationModel.pointing.modify(updatePointing)(o) :: osʹ,
                  ObservationModel.asterism.getOption(o).fold(aids)(aids + _),
                  ObservationModel.target.getOption(o).fold(tids)(tids + _)
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
        oids:     List[Observation.Id],
        editor:   State[ObservationModel, Unit],
        asterism: Input[Asterism.Id],
        target:   Input[Target.Id]
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
          _ <- os.traverse_(o => eventService.publish(ObservationModel.ObservationEvent.updated(o)))
          _ <- as.traverse_(a => eventService.publish(AsterismModel.AsterismEvent.updated(a)))
          _ <- ts.traverse_(t => eventService.publish(TargetModel.TargetEvent.updated(t)))
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

    }
}

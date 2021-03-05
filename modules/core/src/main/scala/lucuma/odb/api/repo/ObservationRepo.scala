// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.core.model.{Asterism, ConstraintSet, Observation, Program, Target}
import lucuma.odb.api.model.{AsterismModel, ConstraintSetModel, InputError, ObservationModel, PlannedTimeSummaryModel, TargetModel}
import lucuma.odb.api.model.ObservationModel.ObservationEvent
import lucuma.odb.api.model.syntax.validatedinput._

import cats.MonadError
import cats.data.State
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import monocle.state.all._

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

  def shareWithConstraintSet(oid: Observation.Id, csid: ConstraintSet.Id): F[ObservationModel]

  def unshareWithConstraintSet(oid: Observation.Id, csid: ConstraintSet.Id): F[ObservationModel]

  def unsetConstraintSet(oid: Observation.Id)(implicit F: MonadError[F, Throwable]): F[ObservationModel]

  def setSubject(
    oids:    List[Observation.Id],
    subject: Option[Either[Asterism.Id, Target.Id]]
  ): F[List[ObservationModel]]

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
          obs.subject.contains(aid.asLeft[Target.Id]) && pid.forall(_ === obs.programId)
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
          obs.subject.contains(tid.asRight[Asterism.Id]) && pid.forall(_ === obs.programId)
        }


      override def insert(newObs: ObservationModel.Create): F[ObservationModel] = {

        def construct(s: PlannedTimeSummaryModel): F[ObservationModel] =
          constructAndPublish { t =>
            (tryNotFindObservation(t, newObs.observationId)     *>
             tryFindProgram(t, newObs.programId)                *>
              newObs.asterismId.traverse(tryFindAsterism(t, _)) *>
              newObs.targetId.traverse(tryFindTarget(t, _))     *>
              newObs.withId(s)
            ).map(createAndInsert(newObs.observationId, _))
          }

        for {
          s <- PlannedTimeSummaryModel.random[F]
          o <- construct(s)
        } yield o
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

      override def setSubject(
        oids:    List[Observation.Id],
        subject: Option[Either[Asterism.Id, Target.Id]]
      ): F[List[ObservationModel]] = {

        val update = tablesRef.modifyState {
          for {
            a  <- subject.flatMap(_.swap.toOption).traverse(TableState.asterism).map(_.sequence)
            t  <- subject.flatMap(_.toOption).traverse(TableState.target).map(_.sequence)
            os <- oids.traverse(TableState.observation).map(_.sequence)
            tb <- State.get
            r  <- (a, t, os).traverseN { (am, tm, oms) =>

              val initialAsterisms = am.fold(Set.empty[Asterism.Id])(m => Set(m.id))
              val initialTargets   = tm.fold(Set.empty[Target.Id])(m => Set(m.id))

              val (updatedAids, updatedTids, observationUpdates) =
                oms.foldLeft((initialAsterisms, initialTargets, List.empty[ObservationModel])) {
                  case ((aids, tids, oms), om) =>
                    (
                      ObservationModel.asterism.getOption(om).fold(aids)(aids + _),  // include any asterism that was there before
                      ObservationModel.target.getOption(om).fold(tids)(tids + _),    // include any target that was there before
                      ObservationModel.subject.set(subject)(om) :: oms
                    )
                }

              val updatedAsterisms = updatedAids.map(tb.asterisms.apply).toList
              val updatedTargets   = updatedTids.map(tb.targets.apply).toList

              Tables.observations
                .mod(_ ++ observationUpdates.map(om => (om.id, om)))
                .as((updatedAsterisms, updatedTargets, observationUpdates))
            }
          } yield r
        }.flatMap(_.liftTo[F])

        for {
          ato <- update
          (as, ts, os) = ato
          _ <- as.traverse_(a => eventService.publish(AsterismModel.AsterismEvent.updated(a)))
          _ <- ts.traverse_(t => eventService.publish(TargetModel.TargetEvent.updated(t)))
          _ <- os.traverse_(o => eventService.publish(ObservationModel.ObservationEvent.updated(o)))
        } yield os
      }

    }
}

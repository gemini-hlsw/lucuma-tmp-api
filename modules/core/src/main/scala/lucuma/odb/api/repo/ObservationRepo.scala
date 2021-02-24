// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{ObservationModel, PlannedTimeSummaryModel}
import lucuma.core.model.{Asterism, ConstraintSet, Observation, Program, Target}
import lucuma.odb.api.model.ObservationModel.ObservationEvent
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import lucuma.odb.api.model.ConstraintSetModel

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

        selectPageFiltered(count, afterGid, includeDeleted) { _ => obs =>
          obs.targets.contains(aid.asLeft[Target.Id]) && pid.forall(_ === obs.programId)
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

        selectPageFiltered(count, afterGid, includeDeleted) { _ => obs =>
          obs.programId === pid
        }

      override def selectPageForTarget(
        tid:            Target.Id,
        pid:            Option[Program.Id],
        count:          Int,
        afterGid:       Option[Observation.Id],
        includeDeleted: Boolean
      ): F[ResultPage[ObservationModel]] =

        selectPageFiltered(count, afterGid, includeDeleted) { _ => obs =>
          // this includes only observations that directly reference a target,
          // but not those referencing an asterism that references the target
          obs.targets.contains(tid.asRight[Asterism.Id]) && pid.forall(_ === obs.programId)
        }


      override def insert(newObs: ObservationModel.Create): F[ObservationModel] = {

        def construct(s: PlannedTimeSummaryModel): F[ObservationModel] =
          constructAndPublish { t =>
            (tryNotFindObservation(t, newObs.observationId) *>
             tryFindProgram(t, newObs.programId)            *>
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
    }
}

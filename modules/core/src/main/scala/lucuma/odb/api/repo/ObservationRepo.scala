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

  def selectAllForAsterism(aid: Asterism.Id, includeDeleted: Boolean = false): F[List[ObservationModel]]

  def selectAllForConstraintSet(csid: ConstraintSet.Id, includeDeleted: Boolean = false): F[List[ObservationModel]]

  def selectAllForProgram(pid: Program.Id, includeDeleted: Boolean = false): F[List[ObservationModel]]

  def selectAllForTarget(tid: Target.Id, includeDeleted: Boolean = false): F[List[ObservationModel]]

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

      private def lookupAll(t: Tables, as: Iterable[Observation.Id]): List[ObservationModel] =
        as.foldLeft(List.empty[ObservationModel]) { (lst, id) =>
          t.observations.get(id).fold(lst)(_ :: lst)
        }

      override def selectAllForAsterism(aid: Asterism.Id, includeDeleted: Boolean): F[List[ObservationModel]] =
        tablesRef
          .get
          .map(_.observations.values.filter(_.targets.contains(Left(aid))).toList)
          .map(deletionFilter(includeDeleted))

      override def selectAllForConstraintSet(csid: ConstraintSet.Id, includeDeleted: Boolean): F[List[ObservationModel]] = 
        tablesRef.get.map { t => 
          lookupAll(t, t.constraintSetObservation.selectRight(csid))
        }.map(deletionFilter(includeDeleted))

      override def selectAllForProgram(pid: Program.Id, includeDeleted: Boolean): F[List[ObservationModel]] =
        tablesRef
          .get
          .map(_.observations.values.filter(_.programId === pid).toList)
          .map(deletionFilter(includeDeleted))

      override def selectAllForTarget(tid: Target.Id, includeDeleted: Boolean): F[List[ObservationModel]] =
        tablesRef
          .get
          // this includes only observations that directly reference a target,
          // but not those referencing an asterism that references the target
          .map(_.observations.values.filter(_.targets.contains(Right(tid))).toList)
          .map(deletionFilter(includeDeleted))

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

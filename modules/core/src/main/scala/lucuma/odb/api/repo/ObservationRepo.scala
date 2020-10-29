// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{ObservationModel, PlannedTimeSummaryModel}
import lucuma.core.model.{Asterism, Observation, Program, Target}
import lucuma.odb.api.model.ObservationModel.ObservationEvent
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._

sealed trait ObservationRepo[F[_]] extends TopLevelRepo[F, Observation.Id, ObservationModel] {

  def selectAllForAsterism(aid: Asterism.Id, includeDeleted: Boolean = false): F[List[ObservationModel]]

  def selectAllForProgram(pid: Program.Id, includeDeleted: Boolean = false): F[List[ObservationModel]]

  def selectAllForTarget(tid: Target.Id, includeDeleted: Boolean = false): F[List[ObservationModel]]

  def insert(input: ObservationModel.Create): F[ObservationModel]

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
      ObservationEvent.apply
    ) with ObservationRepo[F]
      with LookupSupport {

      override def selectAllForAsterism(aid: Asterism.Id, includeDeleted: Boolean): F[List[ObservationModel]] =
        tablesRef
          .get
          .map(_.observations.values.filter(_.asterismId.contains(aid)).toList)
          .map(deletionFilter(includeDeleted))

      override def selectAllForProgram(pid: Program.Id, includeDeleted: Boolean): F[List[ObservationModel]] =
        tablesRef
          .get
          .map(_.observations.values.filter(_.programId === pid).toList)
          .map(deletionFilter(includeDeleted))

      override def selectAllForTarget(tid: Target.Id, includeDeleted: Boolean): F[List[ObservationModel]] =
        tablesRef
          .get
          .map { tables =>
            tables.observations.values.filter { obs =>
              obs.asterismId.exists { aid =>
                tables.asterisms.get(aid).exists(_.targetIds(tid))
              }
            }.toList
          }
          .map(deletionFilter(includeDeleted))

      override def insert(newObs: ObservationModel.Create): F[ObservationModel] = {

        def construct(s: PlannedTimeSummaryModel): F[ObservationModel] =
          constructAndPublish { t =>
            (tryNotFindObservation(t, newObs.observationId) *> tryFindProgram(t, newObs.programId))
              .as(createAndInsert(newObs.observationId, newObs.withId(_, s)))
          }

        for {
          s <- PlannedTimeSummaryModel.random[F]
          o <- construct(s)
        } yield o
      }

    }
}

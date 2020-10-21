// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{AsterismModel, ObservationModel, ProgramModel, TargetModel}
import lucuma.odb.api.model.ObservationModel.{ObservationCreatedEvent, ObservationEditedEvent}
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._

import scala.util.Random
import scala.concurrent.duration._

sealed trait ObservationRepo[F[_]] extends TopLevelRepo[F, ObservationModel.Id, ObservationModel] {

  def selectAllForAsterism(aid: AsterismModel.Id, includeDeleted: Boolean = false): F[List[ObservationModel]]

  def selectAllForProgram(pid: ProgramModel.Id, includeDeleted: Boolean = false): F[List[ObservationModel]]

  def selectAllForTarget(tid: TargetModel.Id, includeDeleted: Boolean = false): F[List[ObservationModel]]

  def insert(input: ObservationModel.Create): F[ObservationModel]

}

object ObservationRepo {

  def create[F[_]](
    tablesRef:    Ref[F, Tables],
    eventService: EventService[F]
  )(implicit F: Sync[F]): ObservationRepo[F] =

    new TopLevelRepoBase[F, ObservationModel.Id, ObservationModel](
      tablesRef,
      eventService,
      Tables.lastObservationId,
      Tables.observations,
      ObservationCreatedEvent.apply,
      ObservationEditedEvent.apply
    ) with ObservationRepo[F]
      with LookupSupport[F] {

      override def selectAllForAsterism(aid: AsterismModel.Id, includeDeleted: Boolean): F[List[ObservationModel]] =
        tablesRef
          .get
          .map(_.observations.values.filter(_.asterismId.contains(aid)).toList)
          .map(deletionFilter(includeDeleted))

      override def selectAllForProgram(pid: ProgramModel.Id, includeDeleted: Boolean): F[List[ObservationModel]] =
        tablesRef
          .get
          .map(_.observations.values.filter(_.programId === pid).toList)
          .map(deletionFilter(includeDeleted))

      override def selectAllForTarget(tid: TargetModel.Id, includeDeleted: Boolean): F[List[ObservationModel]] =
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

        def construct(d: FiniteDuration): F[ObservationModel] =
          modify { t =>
            lookupProgram(t, newObs.programId).fold(
              err => (t, err.asLeft[ObservationModel]),
              _   => createAndInsert(newObs.withId(_, d)).run(t).value.map(_.asRight)
            )
          }

        for {
          d <- F.delay(Random.between(5L, 120L)).map(_.minutes)
          o <- construct(d)
        } yield o
      }

    }
}

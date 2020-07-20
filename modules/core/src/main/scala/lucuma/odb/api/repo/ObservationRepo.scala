// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{Observation, Program}
import lucuma.odb.api.model.Observation.{ObservationCreatedEvent, ObservationEditedEvent}
import cats.{Monad, MonadError}
import cats.effect.concurrent.Ref
import cats.implicits._

sealed trait ObservationRepo[F[_]] extends TopLevelRepo[F, Observation.Id, Observation] {

  def selectAllForProgram(pid: Program.Id, includeDeleted: Boolean = false): F[List[Observation]]

  def insert(input: Observation.Create): F[Observation]

}

object ObservationRepo {

  def create[F[_]: Monad](
    tablesRef:    Ref[F, Tables],
    eventService: EventService[F]
  )(implicit M: MonadError[F, Throwable]): ObservationRepo[F] =

    new TopLevelRepoBase[F, Observation.Id, Observation](
      tablesRef,
      eventService,
      Tables.lastObservationId,
      Tables.observations,
      ObservationCreatedEvent.apply,
      ObservationEditedEvent.apply
    ) with ObservationRepo[F]
      with LookupSupport[F] {

      override def selectAllForProgram(pid: Program.Id, includeDeleted: Boolean = false): F[List[Observation]] =
        tablesRef
          .get
          .map(_.observations.values.filter(_.pid === pid).toList)
          .map(deletionFilter(includeDeleted))

      override def insert(newObs: Observation.Create): F[Observation] =
        modify { t =>
          lookupProgram(t, newObs.pid).fold(
            err => (t, err.asLeft[Observation]),
            _   => createAndInsert(newObs.withId).run(t).value.map(_.asRight)
          )
        }

    }
}

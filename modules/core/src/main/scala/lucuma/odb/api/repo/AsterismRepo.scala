// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{Asterism, Program}
import lucuma.odb.api.model.Asterism.{AsterismCreatedEvent, AsterismEditedEvent}
import cats.{Monad, MonadError}
import cats.data.State
import cats.effect.concurrent.Ref
import cats.implicits._


sealed trait AsterismRepo[F[_]] extends TopLevelRepo[F, Asterism.Id, Asterism] {

  def selectAllForProgram(pid: Program.Id, includeDeleted: Boolean = false): F[List[Asterism]]

  def insert(input: Asterism.Create): F[Asterism]

}

object AsterismRepo {

  def create[F[_]: Monad](
    tablesRef:    Ref[F, Tables],
    eventService: EventService[F]
  )(implicit M: MonadError[F, Throwable]): AsterismRepo[F] =

    new TopLevelRepoBase[F, Asterism.Id, Asterism](
      tablesRef,
      eventService,
      Tables.lastAsterismId,
      Tables.asterisms,
      AsterismCreatedEvent.apply,
      AsterismEditedEvent.apply
    ) with AsterismRepo[F]
      with LookupSupport[F] {

      override def selectAllForProgram(pid: Program.Id, includeDeleted: Boolean = false): F[List[Asterism]] =
        tablesRef.get.map { t =>
          val ids = t.observations.values.filter(_.pid === pid).flatMap(_.asterism.toList).toSet
          ids.foldLeft(List.empty[Asterism]) { (l,i) =>
            t.asterisms.get(i).fold(l)(_ :: l)
          }
        }.map(deletionFilter(includeDeleted))

      private def addAsterism(ac: Asterism.Create): State[Tables, Asterism] =
        for {
          a   <- createAndInsert(ac.withId)
          _   <- Tables.shareAsterism(a, ac.programs.toSet)
        } yield a

      override def insert(input: Asterism.Create): F[Asterism] =
        modify { t =>
          val targets  = input.targets.traverse(lookupTarget(t, _))
          val programs = input.programs.traverse(lookupProgram(t, _))
          (targets, programs)
            .mapN((_, _) => addAsterism(input).run(t).value)
            .fold(
              err => (t, err.asLeft[Asterism]),
              tup => tup.map(_.asRight)
            )
        }

    }
}

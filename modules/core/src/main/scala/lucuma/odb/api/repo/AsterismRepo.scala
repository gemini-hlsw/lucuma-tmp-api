// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{AsterismModel, ProgramModel}
import lucuma.odb.api.model.AsterismModel.{AsterismCreatedEvent, AsterismEditedEvent}
import cats.{Monad, MonadError}
import cats.data.State
import cats.effect.concurrent.Ref
import cats.implicits._


sealed trait AsterismRepo[F[_]] extends TopLevelRepo[F, AsterismModel.Id, AsterismModel] {

  def selectAllForProgram(pid: ProgramModel.Id, includeDeleted: Boolean = false): F[List[AsterismModel]]

  def insert(input: AsterismModel.Create): F[AsterismModel]

}

object AsterismRepo {

  def create[F[_]: Monad](
    tablesRef:     Ref[F, Tables],
    eventService:  EventService[F]
  )(implicit M: MonadError[F, Throwable]): AsterismRepo[F] =

    new TopLevelRepoBase[F, AsterismModel.Id, AsterismModel](
      tablesRef,
      eventService,
      Tables.lastAsterismId,
      Tables.asterisms,
      AsterismCreatedEvent.apply,
      AsterismEditedEvent.apply
    ) with AsterismRepo[F]
      with LookupSupport[F] {

      override def selectAllForProgram(pid: ProgramModel.Id, includeDeleted: Boolean = false): F[List[AsterismModel]] =
        tablesRef.get.map { t =>
          val ids = t.observations.values.filter(_.pid === pid).flatMap(_.asterism.toList).toSet
          ids.foldLeft(List.empty[AsterismModel]) { (l, i) =>
            t.asterisms.get(i).fold(l)(_ :: l)
          }
        }.map(deletionFilter(includeDeleted))

      private def addAsterism(ac: AsterismModel.Create): State[Tables, AsterismModel] =
        for {
          a   <- createAndInsert(ac.withId)
          _   <- Tables.shareAsterism(a, ac.programs.toSet)
        } yield a

      override def insert(input: AsterismModel.Create): F[AsterismModel] =
        modify { t =>
          val targets  = input.targets.traverse(lookupTarget(t, _))
          val programs = input.programs.traverse(lookupProgram(t, _))
          (targets, programs)
            .mapN((_, _) => addAsterism(input).run(t).value)
            .fold(
              err => (t, err.asLeft[AsterismModel]),
              tup => tup.map(_.asRight)
            )
        }

    }
}

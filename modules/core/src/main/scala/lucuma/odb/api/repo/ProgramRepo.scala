// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.implicits._
import cats.MonadError
import cats.data.EitherT
import cats.effect.Ref
import lucuma.core.model.Program
import lucuma.odb.api.model.{Database, Event, InputError, ProgramModel, Table}
import lucuma.odb.api.model.ProgramModel.ProgramEvent
import lucuma.odb.api.model.query.SizeLimitedResult
import lucuma.odb.api.model.syntax.databasestate._
import lucuma.odb.api.model.syntax.eitherinput._

trait ProgramRepo[F[_]] extends TopLevelRepo[F, Program.Id, ProgramModel] {

  def insert(input: ProgramModel.CreateInput): F[ProgramModel.CreateResult]

  def update(input: ProgramModel.UpdateInput): F[SizeLimitedResult[ProgramModel]]
}

object ProgramRepo {

  def create[F[_]](
    databaseRef:  Ref[F, Database],
    eventService: EventService[F]
  )(implicit M: MonadError[F, Throwable]): ProgramRepo[F] =

    new TopLevelRepoBase[F, Program.Id, ProgramModel](
      databaseRef,
      Database.lastProgramId,
      Database.programs.andThen(Table.rows)
    ) with ProgramRepo[F] {

      override def insert(
        input: ProgramModel.CreateInput
      ): F[ProgramModel.CreateResult] = {

        val create = EitherT(
          databaseRef.modify { db =>
            input
              .create
              .run(db)
              .fold(
                err => (db, InputError.Exception(err).asLeft),
                _.map(_.asRight)
              )
          }
        ).rethrowT

        for {
          p <- create
          _ <- eventService.publish(ProgramEvent(_, Event.EditType.Created, p))
        } yield ProgramModel.CreateResult(p)
      }

      override def update(
        input: ProgramModel.UpdateInput
      ): F[SizeLimitedResult[ProgramModel]] =

        for {
          ps <- databaseRef.modifyState(input.editor.flipF).flatMap(_.liftTo[F])
          _ <- ps.allValues.traverse(p => eventService.publish(ProgramModel.ProgramEvent.updated(p)))
        } yield ps

    }

}

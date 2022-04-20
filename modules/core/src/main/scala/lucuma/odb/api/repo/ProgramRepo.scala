// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.implicits._
import cats.MonadError
import cats.data.{EitherT, StateT}
import cats.effect.Ref
import lucuma.core.model.Program
import lucuma.odb.api.model.{Database, EitherInput, Event, InputError, ProgramModel, Table}
import lucuma.odb.api.model.ProgramModel.ProgramEvent
import lucuma.odb.api.model.syntax.databasestate._
import lucuma.odb.api.model.syntax.eitherinput._

trait ProgramRepo[F[_]] extends TopLevelRepo[F, Program.Id, ProgramModel] {

  def selectPageForPrograms(
    pids:           Set[Program.Id],
    count:          Option[Int]        = None,
    afterGid:       Option[Program.Id] = None,
    includeDeleted: Boolean            = false
  ): F[ResultPage[ProgramModel]]

  def insert(input: ProgramModel.Create): F[ProgramModel]

  def edit(input: ProgramModel.Edit): F[ProgramModel]
}

object ProgramRepo {

  def create[F[_]](
    databaseRef:  Ref[F, Database],
    eventService: EventService[F]
  )(implicit M: MonadError[F, Throwable]): ProgramRepo[F] =

    new TopLevelRepoBase[F, Program.Id, ProgramModel](
      databaseRef,
      eventService,
      Database.lastProgramId,
      Database.programs.andThen(Table.rows),
      (editType, model) => ProgramEvent(_, editType, model)
    ) with ProgramRepo[F] {

      override def selectPageForPrograms(
        pids:           Set[Program.Id],
        count:          Option[Int]        = None,
        afterGid:       Option[Program.Id] = None,
        includeDeleted: Boolean            = false
      ): F[ResultPage[ProgramModel]] =

        selectPageFiltered(count, afterGid, includeDeleted) { p => pids(p.id) }

      override def insert(
        input: ProgramModel.Create
      ): F[ProgramModel] = {

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
        } yield p
      }

      override def edit(
        input: ProgramModel.Edit
      ): F[ProgramModel] = {

        val update: StateT[EitherInput, Database, ProgramModel] =
          for {
            initial <- Database.program.lookup(input.programId)
            edited  <- StateT.liftF(input.edit.runS(initial))
            _       <- Database.program.update(input.programId, edited)
          } yield edited

        for {
          p <- databaseRef.modifyState(update.flipF).flatMap(_.liftTo[F])
          _ <- eventService.publish(ProgramModel.ProgramEvent.updated(p))
        } yield p
      }

    }

}

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{Program, Target}
import lucuma.odb.api.model.Program.{ProgramCreatedEvent, ProgramEditedEvent}
import lucuma.odb.api.model.Existence._
import cats.Monad
import cats.implicits._
import cats.MonadError
import cats.effect.concurrent.Ref


trait ProgramRepo[F[_]] extends TopLevelRepo[F, Program.Id, Program] {

  def selectAllForTarget(tid: Target.Id, includeDeleted: Boolean = false): F[List[Program]]

  def insert(input: Program.Create): F[Program]

}

object ProgramRepo {

  def create[F[_]: Monad](
    tablesRef:    Ref[F, Tables],
    eventService: EventService[F]
  )(implicit M: MonadError[F, Throwable]): ProgramRepo[F] =

    new TopLevelRepoBase[F, Program.Id, Program](
      tablesRef,
      eventService,
      Tables.lastProgramId,
      Tables.programs,
      ProgramCreatedEvent.apply,
      ProgramEditedEvent.apply
    ) with ProgramRepo[F]
      with LookupSupport[F] {

      override def selectAllForTarget(tid: Target.Id, includeDeleted: Boolean = false): F[List[Program]] =
        tablesRef.get.flatMap { tables =>
          tables.programTargets.selectLeft(tid).toList.traverse { pid =>
            tables.programs.get(pid).fold(missingProgram(pid))(M.pure)
          }
        }.map(deletionFilter(includeDeleted))

      override def insert(input: Program.Create): F[Program] =
        tablesRef.modifyState(createAndInsert(pid => Program(pid, Present, input.name)))

    }

}
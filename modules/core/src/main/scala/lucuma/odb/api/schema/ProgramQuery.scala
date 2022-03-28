// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.all._
import lucuma.odb.api.repo.OdbCtx
import org.typelevel.log4cats.Logger
import sangria.schema._

trait ProgramQuery {

  import GeneralSchema.ArgumentIncludeDeleted
  import Paging._
  import ProgramSchema.{OptionalListProgramIdArgument, ProgramIdArgument, ProgramType, ProgramConnectionType}
  import context._

  def programs[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "programs",
      fieldType   = ProgramConnectionType[F],
      description = Some("Pages through all requested programs (or all programs if no ids are given)."),
      arguments   = List(
        OptionalListProgramIdArgument.copy(description = "(Optional) listing of programs to retrieve (all programs if empty)".some),
        ArgumentPagingFirst,
        ArgumentPagingCursor,
        ArgumentIncludeDeleted
      ),
      resolve = c =>
        unsafeSelectTopLevelPageFuture(c.pagingProgramId) { gid =>
          c.arg(OptionalListProgramIdArgument).fold(
            c.ctx.odbRepo.program.selectPage(c.pagingFirst, gid, c.includeDeleted)
          ) { pids =>
            c.ctx.odbRepo.program.selectPageForPrograms(pids.toSet, c.pagingFirst, gid, c.includeDeleted)
          }
        }
    )

  def forId[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "program",
      fieldType   = OptionType(ProgramType[F]),
      description = Some("Returns the program with the given id, if any."),
      arguments   = List(ProgramIdArgument, ArgumentIncludeDeleted),
      resolve     = c => c.program(_.select(c.programId, c.includeDeleted))
    )

  def allFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      programs,
      forId
    )

}

object ProgramQuery extends ProgramQuery

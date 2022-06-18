// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.option._
import lucuma.core.model.Program
import lucuma.odb.api.model.{ProgramModel, WhereProgramInput}
import lucuma.odb.api.model.query.SelectResult
import lucuma.odb.api.repo.OdbCtx
import org.typelevel.log4cats.Logger
import sangria.marshalling.circe._
import sangria.schema._

trait ProgramQuery {

  import GeneralSchema.ArgumentIncludeDeleted
  import ProgramSchema.{InputObjectWhereProgram, ProgramIdArgument, ProgramIdType, ProgramType}
  import QuerySchema.{ArgumentOptionLimit, DefaultLimit, SelectResultType}
  import context._

  implicit val ArgumentOptionWhereProgram: Argument[Option[WhereProgramInput]] =
    Argument(
      name         = "WHERE",
      argumentType = OptionInputType(InputObjectWhereProgram),
      description  = "Filters the selection of programs."
    )

  implicit val ArgumentOptionOffsetProgram: Argument[Option[Program.Id]] =
    Argument(
      name         = "OFFSET",
      argumentType = OptionInputType(ProgramIdType),
      description  = "Starts the result set at (or after if not existent) the given program id."
    )

  implicit def ProgramSelectResult[F[_]: Dispatcher: Async: Logger]: ObjectType[Any, SelectResult[ProgramModel]] =
    SelectResultType[ProgramModel]("program", ProgramType[F])

  def programs[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "programs",
      fieldType   = ProgramSelectResult[F],
      description = "Selects the first `LIMIT` matching programs based on the provided `WHERE` parameter, if any.".some,
      arguments   = List(ArgumentOptionWhereProgram, ArgumentOptionOffsetProgram, ArgumentOptionLimit),
      resolve     = c => {
        val where = c.arg(ArgumentOptionWhereProgram).getOrElse(WhereProgramInput.MatchPresent)
        val off   = c.arg(ArgumentOptionOffsetProgram)
        val limit = c.arg(ArgumentOptionLimit).getOrElse(DefaultLimit)
        c.program(_.selectWhere(where, off, limit))
      }
    )

  def program[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "program",
      fieldType   = OptionType(ProgramType[F]),
      description = "Returns the program with the given id, if any.".some,
      arguments   = List(ProgramIdArgument, ArgumentIncludeDeleted),
      resolve     = c => c.program(_.select(c.programId, c.includeDeleted))
    )

  def allFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      programs,
      program
    )

}

object ProgramQuery extends ProgramQuery

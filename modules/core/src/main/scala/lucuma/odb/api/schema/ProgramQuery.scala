// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import lucuma.odb.api.model.{ProgramModel, WhereProgram}
import lucuma.odb.api.model.query.SelectResult
import lucuma.odb.api.repo.OdbCtx
import lucuma.odb.api.schema.ProgramSchema.InputObjectWhereProgram
import lucuma.odb.api.schema.QuerySchema.{DefaultLimit, SelectResultType}
import org.typelevel.log4cats.Logger
import sangria.marshalling.circe._
import sangria.schema._

trait ProgramQuery {

  import GeneralSchema.ArgumentIncludeDeleted
  import ProgramSchema.{ProgramIdArgument, ProgramType}
  import QuerySchema.ArgumentLimit
  import context._

  implicit val ArgumentOptionWhereProgram: Argument[Option[WhereProgram]] =
    Argument(
      name         = "WHERE",
      argumentType = OptionInputType(InputObjectWhereProgram),
      description  = "Filters the selection of programs."
    )

  implicit def ProgramSelectResult[F[_]: Dispatcher: Async: Logger]: ObjectType[Any, SelectResult[ProgramModel]] =
    SelectResultType[ProgramModel]("program", ProgramType[F])

  def programs[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "programs",
      fieldType   = ProgramSelectResult[F],
      description = Some("Selects the first `LIMIT` matching programs based on the provided `WHERE` parameter, if any."),
      arguments   = List(ArgumentOptionWhereProgram, ArgumentLimit),
      resolve = c => c.program(_.selectWhere(c.arg(ArgumentOptionWhereProgram), c.arg(ArgumentLimit).getOrElse(DefaultLimit)))
    )

  def program[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
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
      program
    )

}

object ProgramQuery extends ProgramQuery

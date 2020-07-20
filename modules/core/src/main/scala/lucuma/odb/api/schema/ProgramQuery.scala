// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.repo.OdbRepo
import cats.effect.Effect
import sangria.schema._

trait ProgramQuery {

  import GeneralSchema.ArgumentIncludeDeleted
  import ProgramSchema.{ProgramIdArgument, ProgramType}
  import context._

  def all[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name        = "programs",
      fieldType   = ListType(ProgramType[F]),
      description = Some("Returns all programs (needs pagination)."),
      arguments   = List(ArgumentIncludeDeleted),
      resolve     = c => c.program(_.selectAll(c.includeDeleted))
    )

  def forId[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name        = "program",
      fieldType   = OptionType(ProgramType[F]),
      description = Some("Returns the program with the given id, if any."),
      arguments   = List(ProgramIdArgument, ArgumentIncludeDeleted),
      resolve     = c => c.program(_.select(c.programId, c.includeDeleted))
    )

  def allFields[F[_]: Effect]: List[Field[OdbRepo[F], Unit]] =
    List(
      all,
      forId
    )

}

object ProgramQuery extends ProgramQuery

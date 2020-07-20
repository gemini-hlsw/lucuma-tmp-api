// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.repo.OdbRepo
import cats.effect.Effect
import sangria.schema._

trait TargetQuery {

  import GeneralSchema.ArgumentIncludeDeleted
  import ProgramSchema.ProgramIdArgument
  import TargetSchema.{TargetIdArgument, TargetType}
  import context._

  def forId[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name        = "target",
      fieldType   = OptionType(TargetType[F]),
      description = Some("Returns the target with the given id, if any."),
      arguments   = List(TargetIdArgument, ArgumentIncludeDeleted),
      resolve     = c => c.target(_.select(c.targetId, c.includeDeleted))
    )

  def allForProgram[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name        = "targets",
      fieldType   = ListType(TargetSchema.TargetType[F]),
      description = Some("Return all targets associated with the given program."),
      arguments   = List(ProgramIdArgument, ArgumentIncludeDeleted),
      resolve     = c => c.target(_.selectAllForProgram(c.programId, c.includeDeleted))
    )

  def allFields[F[_]: Effect]: List[Field[OdbRepo[F], Unit]] =
    List(
      allForProgram,
      forId
    )

}

object TargetQuery extends TargetQuery
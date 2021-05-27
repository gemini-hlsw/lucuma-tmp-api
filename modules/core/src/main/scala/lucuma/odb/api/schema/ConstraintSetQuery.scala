// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.repo.OdbRepo
import cats.effect.Effect
import sangria.schema._

trait ConstraintSetQuery {

  import GeneralSchema.ArgumentIncludeDeleted
  import Paging._
  import ProgramSchema.ProgramIdArgument
  import ConstraintSetSchema.{ConstraintSetConnectionType, ConstraintSetIdArgument, ConstraintSetType}
  import context._

  def allForProgram[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name        = "constraintSets",
      fieldType   = ConstraintSetConnectionType,
      description = Some("Returns all constraint sets associated with the given program."),
      arguments   = List(
        ProgramIdArgument,
        ArgumentPagingFirst,
        ArgumentPagingCursor,
        ArgumentIncludeDeleted
      ),
      resolve     = c =>
        unsafeSelectTopLevelPageFuture(c.pagingConstraintSetId) { gid =>
          c.ctx.constraintSet.selectPageForProgram(c.programId, c.pagingFirst, gid, c.includeDeleted)
        }
    )

  def forId[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name        = "constraintSet",
      fieldType   = OptionType(ConstraintSetType[F]),
      description = Some("Returns the constraint set with the given id, if any."),
      arguments   = List(ConstraintSetIdArgument, ArgumentIncludeDeleted),
      resolve     = c => c.constraintSet(_.select(c.constraintSetId, c.includeDeleted))
    )

    def allFields[F[_]: Effect]: List[Field[OdbRepo[F], Unit]] =
      List(allForProgram, forId)
}

object ConstraintSetQuery extends ConstraintSetQuery

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.repo.OdbRepo
import cats.effect.Effect
import sangria.schema._

trait AsterismQuery {

  import GeneralSchema.ArgumentIncludeDeleted
  import ProgramSchema.ProgramIdArgument
  import AsterismSchema.{AsterismIdArgument, AsterismType}
  import context._

  def allForProgram[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name        = "asterisms",
      fieldType   = ListType(AsterismType[F]),
      description = Some("Returns all asterisms associated with the given program."),
      arguments   = List(ProgramIdArgument, ArgumentIncludeDeleted),
      resolve     = c => c.asterism(_.selectAllForProgram(c.programId, c.includeDeleted))
    )

  def forId[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name        = "asterism",
      fieldType   = OptionType(AsterismType[F]),
      description = Some("Returns the asterism with the given id, if any."),
      arguments   = List(AsterismIdArgument, ArgumentIncludeDeleted),
      resolve     = c => c.asterism(_.select(c.asterismId, c.includeDeleted))
    )

  def allFields[F[_]: Effect]: List[Field[OdbRepo[F], Unit]] =
    List(
      allForProgram,
      forId
    )

}

object AsterismQuery extends AsterismQuery

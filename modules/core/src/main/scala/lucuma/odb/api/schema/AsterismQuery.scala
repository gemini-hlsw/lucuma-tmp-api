// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.repo.OdbRepo

import cats.MonadError
import cats.effect.std.Dispatcher
import sangria.schema._

trait AsterismQuery {

  import AsterismSchema.{AsterismConnectionType, AsterismIdArgument, AsterismType}
  import GeneralSchema.ArgumentIncludeDeleted
  import Paging._
  import ProgramSchema.ProgramIdArgument
  import context._

  def allForProgram[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name        = "asterisms",
      fieldType   = AsterismConnectionType[F],
      description = Some("Returns all asterisms associated with the given program."),
      arguments   = List(
        ProgramIdArgument,
        ArgumentPagingFirst,
        ArgumentPagingCursor,
        ArgumentIncludeDeleted
      ),
      resolve     = c =>
        unsafeSelectTopLevelPageFuture(c.pagingAsterismId) { gid =>
          c.ctx.asterism.selectPageForProgram(c.programId, c.pagingFirst, gid, c.includeDeleted)
        }
    )

  def forId[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name        = "asterism",
      fieldType   = OptionType(AsterismType[F]),
      description = Some("Returns the asterism with the given id, if any."),
      arguments   = List(AsterismIdArgument, ArgumentIncludeDeleted),
      resolve     = c => c.asterism(_.select(c.asterismId, c.includeDeleted))
    )

  def allFields[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): List[Field[OdbRepo[F], Unit]] =
    List(
      allForProgram,
      forId
    )

}

object AsterismQuery extends AsterismQuery

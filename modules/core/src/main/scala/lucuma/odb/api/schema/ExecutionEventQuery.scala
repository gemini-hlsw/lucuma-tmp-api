// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.option._
import lucuma.core.model.ExecutionEvent
import lucuma.odb.api.model.{ExecutionEventModel, WhereExecutionEventInput}
import lucuma.odb.api.model.query.SizeLimitedResult
import lucuma.odb.api.repo.OdbCtx
import lucuma.odb.api.schema.ExecutionEventSchema.{ExecutionEventIdType, ExecutionEventType}
import org.typelevel.log4cats.Logger
import sangria.marshalling.circe._
import sangria.schema._

trait ExecutionEventQuery {

  import context._
  import ExecutionEventSchema.InputObjectWhereExecutionEvent
  import QuerySchema.{ArgumentOptionLimit, SelectResultType}

  implicit val ArgumentOptionWhereExecutionEvent: Argument[Option[WhereExecutionEventInput]] =
    Argument(
      name         = "WHERE",
      argumentType = OptionInputType(InputObjectWhereExecutionEvent),
      description  = "Filters the selection of execution events"
    )

  implicit val ArgumentOptionOffsetExecutionEvent: Argument[Option[ExecutionEvent.Id]] =
    Argument(
      name         = "OFFSET",
      argumentType = OptionInputType(ExecutionEventIdType),
      description  = "Starts the result set at (or after if not existent) the given execution event id."
    )

  implicit def ExecutionEventSelectResult[F[_]: Dispatcher: Async: Logger]: ObjectType[Any, SizeLimitedResult[ExecutionEventModel]] =
    SelectResultType[ExecutionEventModel]("ExecutionEvent", ExecutionEventType[F])

  def executionEvents[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "executionEvents",
      fieldType   = ExecutionEventSelectResult[F],
      description = "Selects the first `LIMIT` matching execution events based on the provided `WHERE` parameter, if any.".some,
      arguments   = List(
        ArgumentOptionWhereExecutionEvent,
        ArgumentOptionOffsetExecutionEvent,
        ArgumentOptionLimit
      ),
      resolve     = c => {
        val where = c.arg(ArgumentOptionWhereExecutionEvent).getOrElse(WhereExecutionEventInput.MatchAll)
        val off   = c.arg(ArgumentOptionOffsetExecutionEvent)
        val limit = c.resultSetLimit
        c.executionEvent(_.selectWhere(where, off, limit))
      }
    )

  def allFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      executionEvents
    )
}

object ExecutionEventQuery extends ExecutionEventQuery
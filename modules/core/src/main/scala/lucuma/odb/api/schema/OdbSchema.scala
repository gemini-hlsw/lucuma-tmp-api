// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.InputError
import lucuma.odb.api.repo.OdbRepo

import cats.effect.Async
import cats.Parallel
import cats.effect.std.Dispatcher
import sangria.execution.{ExceptionHandler, HandledException}
import sangria.schema.Schema
import lucuma.odb.itc.Itc

/**
 * The entry point schema definition, which just references the available
 * queries and mutations (and eventually subscriptions).
 */
object OdbSchema {

  /**
   * Handle InputError.Exception to create a separate error message for each
   * violation that was encountered.
   */
  val exceptionHandler: ExceptionHandler =
    ExceptionHandler(
      onException = {
        case (m, InputError.Exception(nec)) =>
          HandledException.multiple(nec.toNonEmptyVector.toVector.map { e =>
            (e.message, Map.empty[String, m.Node], Nil)
          })
      }
    )

  def apply[F[_]: Parallel: Dispatcher: Async: Itc]: Schema[OdbRepo[F], Unit] =
    Schema(
      QueryType[F],
      Some(MutationType[F]),
      Some(SubscriptionType[F]),
      additionalTypes =
        InstrumentConfigSchema.implementations[F] ++
        ExecutionConfigSchema.implementations[F]
    )

}

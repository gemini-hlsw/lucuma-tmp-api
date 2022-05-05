// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.InputError
import cats.effect.Async
import cats.effect.std.Dispatcher
import lucuma.odb.api.repo.OdbCtx
import org.typelevel.log4cats.Logger
import sangria.execution.{ExceptionHandler, HandledException}
import sangria.schema.Schema

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

  def apply[F[_]: Dispatcher: Async: Logger](
    testing: Boolean
  ): Schema[OdbCtx[F], Unit] =
    Schema(
      QueryType[F],
      Some(MutationType[F](testing)),
      Some(SubscriptionType[F]),
      additionalTypes =
        ManualConfigSchema.manualConfigImplementations[F] ++
        ExecutionSchema.executionConfigImplementations[F]
    )

}

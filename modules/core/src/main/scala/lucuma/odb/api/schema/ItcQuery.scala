// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.itc.client.ItcClient
import lucuma.odb.api.repo.OdbRepo
import cats.MonadError
import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.all._
import org.typelevel.log4cats.Logger
import sangria.schema._

trait ItcQuery {

  def itc[F[_]: Dispatcher: Async: Logger](implicit M: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name        = "itc",
      fieldType   = StringType,
      description = "Count of results".some,
      resolve     = c => {
        implicitly[Dispatcher[F]].unsafeToFuture(
           ItcClient.query[F]().map(_.toString)
        )
      }
    )

  def allFields[F[_]: Dispatcher: Async: Logger](implicit M: MonadError[F, Throwable]): List[Field[OdbRepo[F], Unit]] =
    List(
      itc
    )
}

object ItcQuery extends ItcQuery

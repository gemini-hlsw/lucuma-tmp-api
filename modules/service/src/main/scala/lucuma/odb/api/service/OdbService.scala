// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.service

import lucuma.odb.api.schema.OdbSchema
import lucuma.odb.api.repo.OdbRepo
import cats.implicits._
import cats.effect.{Async, ConcurrentEffect, ContextShift, IO}
import fs2.Stream
import io.chrisdavenport.log4cats.Logger
import io.circe._
import org.log4s.getLogger
import sangria.execution._
import sangria.marshalling.circe._
import sangria.streaming
import sangria.streaming.SubscriptionStream

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.control.NonFatal
import scala.util.{Failure, Success}


trait OdbService[F[_]] {

  def query(request: ParsedGraphQLRequest): F[Either[Throwable, Json]]

  def subscribe(request: ParsedGraphQLRequest): F[Stream[F, Either[Throwable, Json]]]

}


object OdbService {

  private[this] val logger = getLogger

  def apply[F[_]: Logger](
    odb: OdbRepo[F]
  )(
    implicit F: ConcurrentEffect[F], cs: ContextShift[F]
  ): OdbService[F] =

    new OdbService[F] {

      def info(m: String): F[Unit] =
        F.delay(logger.info(m))

      override def query(request: ParsedGraphQLRequest): F[Either[Throwable, Json]] =

        F.async { (cb: Either[Throwable, Json] => Unit) =>
          Executor.execute(
            schema           = OdbSchema[F],
            queryAst         = request.query,
            userContext      = odb,
            operationName    = request.op,
            variables        = request.vars.getOrElse(Json.fromJsonObject(JsonObject())),
            exceptionHandler = OdbSchema.exceptionHandler
          ).onComplete {
            case Success(value) => cb(Right(value))
            case Failure(error) => cb(Left(error))
          }
        }.attempt

      override def subscribe(request: ParsedGraphQLRequest): F[Stream[F, Either[Throwable, Json]]] = {

        implicit val subStream: SubscriptionStream[Stream[F, *]] =
          streaming.fs2.fs2SubscriptionStream[F](ConcurrentEffect[F], scala.concurrent.ExecutionContext.global)

        import sangria.execution.ExecutionScheme.Stream

        Async.fromFuture {
          Async.liftIO {
            IO.delay {
              Executor.prepare(
                schema           = OdbSchema[F],
                queryAst         = request.query,
                userContext      = odb,
                operationName    = request.op,
                variables        = request.vars.getOrElse(Json.fromJsonObject(JsonObject())),
                exceptionHandler = OdbSchema.exceptionHandler
              ).map { preparedQuery =>
                preparedQuery
                  .execute()
                  .evalTap(n => info(s"Subscription event: ${n.printWith(Printer.spaces2)}"))
                  .map(_.asRight[Throwable])
                  .recover { case NonFatal(error) => error.asLeft[Json] }
              }
            }
          }
        }

      }

    }
}
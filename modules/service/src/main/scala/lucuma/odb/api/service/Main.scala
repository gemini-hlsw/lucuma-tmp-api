// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.service

import lucuma.odb.api.repo.OdbRepo
import cats.effect.{Async, ExitCode, IO, IOApp, Resource}
import cats.implicits._
import fs2.Stream
import org.http4s.implicits._
import org.http4s.HttpApp
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.server.middleware.Logger
import org.typelevel.log4cats.{Logger => Log4CatsLogger}
import org.typelevel.log4cats.slf4j.Slf4jLogger
import lucuma.graphql.routes.SangriaGraphQLService
import lucuma.graphql.routes.GraphQLService
import lucuma.odb.api.schema.OdbSchema
import cats.effect.std.Dispatcher
import lucuma.graphql.routes.Routes
import org.http4s.HttpRoutes
// TODO: SSO
//import lucuma.sso.client.SsoClient
//import lucuma.core.model.User
import org.http4s.headers.Authorization
import org.http4s.server.websocket.WebSocketBuilder2
import cats.data.OptionT

// #server
object Main extends IOApp {

  def httpApp[F[_]: Log4CatsLogger: Async](
    odb:        OdbRepo[F],
// TODO: SSO
//    userClient: SsoClient[F, User],
  ): Resource[F, WebSocketBuilder2[F] => HttpApp[F]] =
    Dispatcher[F].map { implicit d =>

      // Our schema is constant for now
      val schema = OdbSchema[F]

      wsb => Logger.httpApp(logHeaders = true, logBody = false) {

          // Our GraphQL service, computed per-request.
          // For now we check log the user, if any, but it's not required.
          def graphQLService(auth: Option[Authorization]): F[Option[GraphQLService[F]]] =
            OptionT
              .fromOption[F](auth)
              // TODO: SSO
//              .flatMap(a => OptionT(userClient.get(a)))
              .value
              .as(new SangriaGraphQLService(schema, odb, OdbSchema.exceptionHandler).some)

              // TODO: SSO
//              .flatMap { ou =>
//                Log4CatsLogger[F].info(s"GraphQL request (user=$ou).").as {
//                  new SangriaGraphQLService(schema, odb, OdbSchema.exceptionHandler).some
//                }
//              }

          // Our GraphQL routes
          val graphQLRoutes: HttpRoutes[F] =
            Routes.forService[F](graphQLService, wsb, "odb")

          // Done!
          graphQLRoutes.orNotFound

        }

    }


  def stream[F[_]: Log4CatsLogger: Async](
    odb: OdbRepo[F],
    cfg: Config
  ): Stream[F, Nothing] = {
    // Spin up the server ...
    for {
      // TODO: SSO
//      sso       <- Stream.resource(cfg.ssoClient[F])
//      userClient = sso.map(_.user)
      httpApp   <- Stream.resource(httpApp(odb)) //, userClient)) // TODO: SSO
      exitCode  <- BlazeServerBuilder[F]
        .bindHttp(cfg.port, "0.0.0.0")
        .withHttpWebSocketApp(httpApp)
        .serve
    } yield exitCode
  }.drain

  def run(args: List[String]): IO[ExitCode] =
    for {
      cfg  <- Config.fromCiris.load(Async[IO])
      log  <- Slf4jLogger.create[IO]
      odb  <- OdbRepo.create[IO]
      _    <- Init.initialize(odb)
      _    <- stream(odb, cfg)(log, Async[IO]).compile.drain
    } yield ExitCode.Success
}


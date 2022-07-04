// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.service

import lucuma.odb.api.repo.{OdbCtx, OdbRepo}
import cats.effect.{Async, ExitCode, IO, IOApp, Resource}
import cats.effect.std.Dispatcher
import cats.implicits._
import doobie.h2._
import doobie.implicits._
import doobie.util.ExecutionContexts
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
import lucuma.graphql.routes.Routes
import lucuma.itc.client.ItcClient
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
    ctx:        OdbCtx[F],
// TODO: SSO
//    userClient: SsoClient[F, User],
    testing:    Boolean
  ): Resource[F, WebSocketBuilder2[F] => HttpApp[F]] =
    Dispatcher[F].map { implicit d =>

      // Our schema is constant for now
      val schema = OdbSchema[F](testing)

      wsb => Logger.httpApp(logHeaders = true, logBody = false) {

          // Our GraphQL service, computed per-request.
          // For now we check log the user, if any, but it's not required.
          def graphQLService(auth: Option[Authorization]): F[Option[GraphQLService[F]]] =
            OptionT
              .fromOption[F](auth)
              // TODO: SSO
//              .flatMap(a => OptionT(userClient.get(a)))
              .value
              .as(new SangriaGraphQLService(schema, ctx, OdbSchema.exceptionHandler).some)

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

//  def setupDatabase: ConnectionIO[Unit] = {
//    sql"""
//      CREATE TABLE foo (
//        VALUE INTEGER NOT NULL
//      )
//    """.update.run.void
//  }

  def transactor[F[_]: Async]: Resource[F, H2Transactor[F]] =
    for {
      ce <- ExecutionContexts.fixedThreadPool[F](32)
      xa <- H2Transactor.newH2Transactor[F](
        "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1",
        "sa",
        "",
        ce
      )
    } yield xa

  def stream[F[_]: Log4CatsLogger: Async](
    itc:  ItcClient[F],
    repo: OdbRepo[F],
    cfg:  Config
  ): Stream[F, ExitCode] =
    // Spin up the server ...
    for {
      // TODO: SSO
//      sso       <- Stream.resource(cfg.ssoClient[F])
//      userClient = sso.map(_.user)
      xa        <- Stream.resource(transactor[F])
      _         <- Stream.eval(lucuma.odb.api.h2.PlannedTimeDao.init.transact(xa))
      ctx        = OdbCtx.create[F](itc, repo, xa)
      httpApp   <- Stream.resource(httpApp(ctx, testing = false)) //, userClient)) // TODO: SSO
      exitCode  <- BlazeServerBuilder[F]
        .bindHttp(cfg.port, "0.0.0.0")
        .withHttpWebSocketApp(httpApp)
        .serve
    } yield exitCode

  def run(args: List[String]): IO[ExitCode] =
    for {
      cfg  <- Config.fromCiris.load(Async[IO])
      log  <- Slf4jLogger.create[IO]
      itc  <- ItcClient.create(cfg.itc)(Async[IO], log)
      rpo  <- OdbRepo.create[IO]
      _    <- Init.initialize(rpo)
//      ctx   = OdbCtx.create[IO](itc, rpo)
      ext  <- stream(itc, rpo, cfg)(log, Async[IO]).compile.lastOrError
    } yield ext
}


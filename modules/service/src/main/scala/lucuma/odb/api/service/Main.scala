// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.service

import java.util.concurrent._
import lucuma.odb.api.repo.OdbRepo
import cats.effect.{Concurrent, ConcurrentEffect, ExitCode, IO, IOApp}
import cats.implicits._
import fs2.Stream
import org.http4s.implicits._
import org.http4s.HttpApp
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.Logger
import org.http4s.server.staticcontent._
import org.typelevel.log4cats.{Logger => Log4CatsLogger}
import org.typelevel.log4cats.slf4j.Slf4jLogger
import lucuma.core.model.User
import lucuma.sso.client.SsoClient

import scala.concurrent.ExecutionContext.global
import cats.effect.Temporal

// #server
object Main extends IOApp {

  def stream[F[_]: ConcurrentEffect : ContextShift: Log4CatsLogger](
    odb: OdbRepo[F],
    cfg: Config
  )(implicit T: Temporal[F]): Stream[F, Nothing] = {
    val blockingPool = Executors.newFixedThreadPool(4)
    val blocker      = Blocker.liftExecutorService(blockingPool)
    val odbService   = OdbService.apply[F](odb)

    def app(userClient: SsoClient[F, User]): HttpApp[F] =
      Logger.httpApp(logHeaders = true, logBody = false)((

        // Routes for static resources, ie. GraphQL Playground
        resourceService[F](ResourceService.Config("/assets", blocker)) <+>

        // Routes for the ODB GraphQL service
        Routes.forService[F](odbService, userClient)

      ).orNotFound)

    // Spin up the server ...
    for {
      sso       <- Stream.resource(cfg.ssoClient[F])
      userClient = sso.map(_.user)
      exitCode  <- BlazeServerBuilder[F](global)
        .bindHttp(cfg.port, "0.0.0.0")
        .withHttpApp(app(userClient))
        .withWebSockets(true)
        .serve
    } yield exitCode
  }.drain

  def run(args: List[String]): IO[ExitCode] =
    for {
      cfg  <- Config.fromCiris.load[IO]
      log  <- Slf4jLogger.create[IO]
      odb  <- OdbRepo.create[IO](Concurrent[IO], log)
      _    <- Init.initialize(odb)
      _    <- stream(odb, cfg)(ConcurrentEffect[IO], IO.contextShift(global), log, IO.timer(global)).compile.drain
    } yield ExitCode.Success
}


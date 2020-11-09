// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.service

import java.util.concurrent._

import lucuma.odb.api.repo.OdbRepo
import cats.effect.{Blocker, Concurrent, ConcurrentEffect, ContextShift, ExitCode, IO, IOApp, Timer}
import cats.implicits._
import fs2.Stream
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.Logger
import org.http4s.server.staticcontent._
import io.chrisdavenport.log4cats.{ Logger => Log4CatsLogger}
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import scala.concurrent.ExecutionContext.global

// #server
object Main extends IOApp {

  def stream[F[_]: ConcurrentEffect : ContextShift: Log4CatsLogger](odb: OdbRepo[F], port: Int)(implicit T: Timer[F]): Stream[F, Nothing] = {
    val blockingPool = Executors.newFixedThreadPool(4)
    val blocker      = Blocker.liftExecutorService(blockingPool)
    val odbService   = OdbService.apply[F](odb)

    val httpApp0 = (
      // Routes for static resources, ie. GraphQL Playground
      resourceService[F](ResourceService.Config("/assets", blocker)) <+>
      // Routes for the ODB GraphQL service
      Routes.forService[F](odbService)
    ).orNotFound

    val httpApp = Logger.httpApp(logHeaders = true, logBody = false)(httpApp0)

    // Spin up the server ...
    for {
      exitCode <- BlazeServerBuilder[F](global)
        .bindHttp(port, "0.0.0.0")
        .withHttpApp(httpApp)
        .withWebSockets(true)
        .serve
    } yield exitCode
  }.drain

  def run(args: List[String]): IO[ExitCode] =
    for {
      log  <- Slf4jLogger.create[IO]
      odb  <- OdbRepo.create[IO](Concurrent[IO], log)
      port <- IO(sys.env.getOrElse("PORT", "8080").toInt) // Heroku provides binding port in PORT env variable.
      _    <- Init.initialize(odb)
      _    <- stream(odb, port)(ConcurrentEffect[IO], IO.contextShift(global), log, IO.timer(global)).compile.drain
    } yield ExitCode.Success
}


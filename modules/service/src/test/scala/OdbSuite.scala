// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test

import cats.implicits._
import cats.effect._
import clue.GraphQLOperation
import clue.TransactionalClient
import clue.http4sjdk.Http4sJDKBackend
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.literal._
import lucuma.core.model.User
import lucuma.odb.api.repo.OdbRepo
import lucuma.odb.api.service.Init
import lucuma.odb.api.service.OdbService
import lucuma.odb.api.service.Routes
import lucuma.sso.client.SsoClient
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.headers.Authorization
import org.http4s.implicits._
import org.http4s.server.Server
import org.http4s.{Uri => _, _}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import sttp.model.Uri

import java.net.http.HttpClient
import scala.concurrent.ExecutionContext
import munit.CatsEffectSuite

/**
 * Mixin that allows execution of GraphQL operations on a per-suite instance of the Odb, shared
 * among all tests.
 */
trait OdbSuite extends CatsEffectSuite {

  private implicit val log: Logger[IO] =
    Slf4jLogger.getLogger[IO]

  private val ssoClient: SsoClient[IO, User] =
    new SsoClient[IO, User] {
      def find(req: Request[IO]): IO[Option[User]] = IO.pure(None)
      def get(authorization: Authorization): IO[Option[User]] = IO.pure(None)
      def map[B](f: User => B): SsoClient[IO,B] = ???
    }

  private val app: IO[HttpApp[IO]] =
    OdbRepo
      .create[IO]
      .flatTap(Init.initialize(_))
      .map(OdbService(_))
      .map(Routes.forService(_, ssoClient).orNotFound)

  private val server: Resource[IO, Server] =
    Resource.make(IO.println("  • Server starting..."))(_ => IO.println("  • Server stopped.")) *>
    Resource.eval(app).flatMap { app =>
      BlazeServerBuilder[IO](ExecutionContext.global)
        .withHttpApp(app)
        .bindAny()
        .resource
        .flatTap(_ => Resource.eval(IO.println("  • Server started.")))
    }

  private def transactionalClient(svr: Server): Resource[IO, TransactionalClient[IO, Nothing]] =
    for {
      xbe <- Http4sJDKBackend.fromHttpClient[IO](HttpClient.newBuilder().build())
      uri  = Uri.parse((svr.baseUri / "odb").renderString).toOption.get
      xc  <- Resource.eval(TransactionalClient.of[IO, Nothing](uri)(implicitly, xbe, implicitly))
    } yield xc

  case class Operation(
    document: String
  ) extends GraphQLOperation[Nothing] {
    type Data      = Json
    type Variables = Json
    val varEncoder:  Encoder[Json] = implicitly
    val dataDecoder: Decoder[Json] = implicitly
  }

  private val serverFixture: Fixture[Server] =
    ResourceSuiteLocalFixture("server", server)

  override def munitFixtures = List(serverFixture)

  /** Run a transactional query and check that the response is as expected. */
  def testTransactional(query: String, expected: Json, variables: Option[Json] = None) =
    test(query.linesIterator.dropWhile(_.trim.isEmpty).next().trim + " ...") {
      Resource.eval(IO(serverFixture()))
        .flatMap(transactionalClient)
        .use { xc =>
          variables match {
            case Some(j) => xc.request(Operation(query)).apply(j)
            case None    => xc.request(Operation(query)).apply
          }
        }
        .map(_.spaces2)
        .assertEquals(expected.spaces2) // by comparing strings we get more useful errors
  }

}


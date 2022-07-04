// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test

import cats.effect._
import cats.implicits._
import clue.ApolloWebSocketClient
import clue.ResponseException
import clue.GraphQLOperation
import clue.PersistentStreamingClient
import clue.TransactionalClient
import io.circe.Json
import clue.http4s.{Http4sBackend, Http4sWSBackend}
import doobie.Transactor
import doobie.h2.H2Transactor
import doobie.implicits._
import doobie.util.ExecutionContexts
import io.circe.literal._
import lucuma.itc.client.ItcClient
import lucuma.odb.api.repo.OdbCtx
// TODO: SSO
//import lucuma.core.model.User
import lucuma.odb.api.repo.OdbRepo
import lucuma.odb.api.service.Main
// TODO: SSO
//import lucuma.sso.client.SsoClient
import munit.CatsEffectSuite
import org.http4s.{Uri => Http4sUri, _}
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.jdkhttpclient.{JdkHttpClient, JdkWSClient}
import org.http4s.implicits.http4sLiteralsSyntax
import org.http4s.headers.Authorization
import org.http4s.server.Server
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration._
import org.http4s.server.websocket.WebSocketBuilder2

/**
 * Mixin that allows execution of GraphQL operations on a per-suite instance of the Odb, shared
 * among all tests.
 */
trait OdbSuite extends CatsEffectSuite {

  private implicit val log: Logger[IO] =
    Slf4jLogger.getLoggerFromClass(getClass)

  // TODO: SSO
//  private val ssoClient: SsoClient[IO, User] =
//    new SsoClient[IO, User] {
//      def find(req: Request[IO]): IO[Option[User]] = IO.pure(None)
//      def get(authorization: Authorization): IO[Option[User]] = IO.pure(None)
//      def require(req: Request[IO])(f: User => IO[Response[IO]]): IO[Response[IO]] = ???
//      def map[B](f: User => B): SsoClient[IO,B] = ???
//      def filter(f: User => Boolean): SsoClient[IO,User] = ???
//      def collect[B](f: PartialFunction[User,B]): SsoClient[IO,B] = ???
//    }

  private def transactor[F[_]: Async]: Resource[F, H2Transactor[F]] =
    for {
      ce <- ExecutionContexts.fixedThreadPool[F](32)
      xa <- H2Transactor.newH2Transactor[F](
        "jdbc:h2:mem:;DB_CLOSE_DELAY=-1",
        "sa",
        "",
        ce
      )
    } yield xa

  private val httpApp: Resource[IO, WebSocketBuilder2[IO] => HttpApp[IO]] = {
    def setupContext(xa: Transactor[IO]): IO[OdbCtx[IO]] =
      for {
        itc <- ItcClient.create[IO](uri"https://itc-staging.herokuapp.com/itc")
        rpo <- OdbRepo.create[IO].flatTap(TestInit.initialize(_))
      } yield OdbCtx.create(itc, rpo, xa)

    for {
      xa  <- transactor[IO]
      _   <- Resource.eval(lucuma.odb.api.h2.PlannedTimeDao.init.transact(xa))
      ctx <- Resource.eval(setupContext(xa))
      app <- Main.httpApp(ctx, testing = true)
    } yield app

  }

  private val server: Resource[IO, Server] =
//    Resource.make(IO.println("  • Server starting..."))(_ => IO.println("  • Server stopped.")) *>
    httpApp.flatMap { app =>
      BlazeServerBuilder[IO]
        .withHttpWebSocketApp(app)
        .bindAny()
        .resource
//        .flatTap(_ => Resource.eval(IO.println("  • Server started.")))
    }

  private def transactionalClient(svr: Server): Resource[IO, TransactionalClient[IO, Nothing]] =
    for {
      xbe <- JdkHttpClient.simple[IO].map(Http4sBackend[IO](_))
      uri  = svr.baseUri / "odb"
      xc  <- Resource.eval(TransactionalClient.of[IO, Nothing](uri, headers = Headers(Authorization(Credentials.Token(AuthScheme.Bearer, "123"))))(Async[IO], xbe, Logger[IO]))
    } yield xc

  private def streamingClient(svr: Server): Resource[IO, PersistentStreamingClient[IO, Nothing, _, _]] =
    for {
      sbe <- JdkWSClient.simple[IO].map(Http4sWSBackend[IO](_))
      uri  = (svr.baseUri / "ws").copy(scheme = Some(Http4sUri.Scheme.unsafeFromString("ws")))
      sc  <- Resource.eval(ApolloWebSocketClient.of(uri)(Async[IO], Logger[IO], sbe))
      ps   = Map("Authorization" -> Json.fromString("Bearer 123"))
      _   <- Resource.make(sc.connect() *> sc.initialize(ps))(_ => sc.terminate() *> sc.disconnect())
    } yield sc

  case class Operation(
    document: String
  ) extends GraphQLOperation[Nothing] {
    type Data       = Json
    type Variables  = Json
    val varEncoder  = implicitly
    val dataDecoder = implicitly
  }

  private val serverFixture: Fixture[Server] =
    ResourceSuiteLocalFixture("server", server)

  override def munitFixtures = List(serverFixture)

  sealed trait ClientOption extends Product with Serializable {

    def prefix: String =
      this match {
        case ClientOption.Http => "[http]"
        case ClientOption.Ws   => "[ws]  "
      }

    def connection: Server => Resource[IO, TransactionalClient[IO, Nothing]] =
      this match {
        case ClientOption.Http => transactionalClient
        case ClientOption.Ws   => streamingClient
      }

  }

  object ClientOption {
    case object Http extends ClientOption
    case object Ws   extends ClientOption

    val All: List[ClientOption] = List(Http, Ws)
  }

  /** Run a query and ensure that the responses are as expected. */
  def queryTest(
    query:     String,
    expected:  Json,
    variables: Option[Json] = None,
    clients:   List[ClientOption] = List(ClientOption.Http)
  ): Unit =
    queryTestImpl(query, expected.asRight, variables, clients)

  def queryTestFailure(
    query:     String,
    errors:    List[String],
    variables: Option[Json] = None,
    clients:   List[ClientOption] = List(ClientOption.Http)
  ): Unit =
    queryTestImpl(query, errors.asLeft, variables, clients)

  private def queryTestImpl(
    query:     String,
    expected:  Either[List[String], Json],
    variables: Option[Json],
    clients:   List[ClientOption]
  ): Unit = {

    def go(client: ClientOption): Unit = {
      val suffix = query.linesIterator.dropWhile(_.trim.isEmpty).next().trim + " ..."
      test(s"${client.prefix} $suffix") {
        Resource.eval(IO(serverFixture()))
          .flatMap(client.connection)
          .use { conn =>
            val req = conn.request(Operation(query))
            val op  = variables.fold(req.apply)(req.apply)

            expected.fold(errors => {
              op.intercept[ResponseException]
                .map(e => e.asGraphQLErrors.toList.flatten.map(_.message))
                .assertEquals(errors)
            }, success => {
              op.map(_.spaces2)
                .assertEquals(success.spaces2) // by comparing strings we get more useful errors
            })
          }
      }
    }

    clients.foreach(go)
  }

  def subscriptionTest(
    query:     String,
    mutations: Either[List[(String, Option[Json])], IO[Unit]],
    expected:  List[Json], variables: Option[Json] = None
  ): Unit = {
    val suffix = query.linesIterator.dropWhile(_.trim.isEmpty).next().trim + " ..."
    test(s"[sub]  $suffix") {
      Resource.eval(IO(serverFixture()))
        .flatMap(streamingClient)
        .use { conn =>
          val req = conn.subscribe(Operation(query))
          for {
            _   <- log.debug("*** ----- about to start stream fiber")
            tup <- variables.fold(req.apply)(req.apply).allocated
            (stream, close) = tup
            fib <- stream.compile.toList.start
            _   <- log.debug("*** ----- pausing a bit")
            _   <- IO.sleep(500.millis)
            _   <- log.debug("*** ----- running mutations")
            _   <- mutations.fold(_.traverse_ { case (query, vars) =>
                     val req = conn.request(Operation(query))
                     vars.fold(req.apply)(req.apply)
                   }, identity)
            _   <- log.debug("*** ----- pausing a bit")
            _   <- IO.sleep(200.millis)
            _   <- log.debug("*** ----- stopping subscription")
            _   <- close
            _   <- log.debug("*** ----- joining fiber")
            obt <- fib.joinWithNever
          } yield assertEquals(obt.map(_.spaces2), expected.map(_.spaces2))  // by comparing strings we get more useful errors
        }
    }
  }

}

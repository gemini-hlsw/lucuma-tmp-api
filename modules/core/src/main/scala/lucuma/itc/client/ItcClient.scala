// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.syntax.all._
import cats.effect.{Async, Ref, Resource}
import clue.TransactionalClient
import clue.http4sjdk.Http4sJDKBackend
import io.circe.syntax._
import lucuma.core.model.Target
import lucuma.odb.api.model.ObservationModel
import org.http4s.Uri
import org.typelevel.log4cats.Logger

class ItcClient[F[_]: Async: Logger](
  uri:   Uri,
  // For now, we'll just cache results forever (i.e., until the next restart)
  cache: Ref[F, Map[ItcSpectroscopyInput, Option[ItcSpectroscopyResult]]]
) {

  val resource: Resource[F, TransactionalClient[F, Unit]] =
    for {
      b <- Http4sJDKBackend[F]
      c <- Resource.eval(TransactionalClient.of[F, Unit](uri)(Async[F], b, Logger[F]))
    } yield c

  def query(
    o: ObservationModel,
    t: Target,
    useCache: Boolean
  ): F[Option[ItcSpectroscopyResult]] = {

    def callItc(in: ItcSpectroscopyInput): F[Option[ItcSpectroscopyResult]] =
      for {
        x <- resource.use(_.request(ItcQuery)(in))
        r  = x.headOption
        _ <- cache.update(_ + (in -> r))
      } yield r

    val input: Option[ItcSpectroscopyInput] = ItcSpectroscopyInput.fromObservation(o, t)

    for {
      _    <- Logger[F].info(s"ITC Input:\n${input.asJson.spaces2}")
      cval <- if (useCache) input.flatTraverse { in => cache.get.map(_.get(in)) } else Async[F].pure(None)
      res  <- cval.fold(input.flatTraverse(callItc))(Async[F].pure)
      _    <- Logger[F].info(s"ITC Result (${cval.fold("from ITC")(_ => "from cache")}):\n$res")
    } yield res
  }

}

object ItcClient {

  def create[F[_]: Async: Logger](
    uri: Uri
  ): F[ItcClient[F]] =
    Ref.of[F, Map[ItcSpectroscopyInput, Option[ItcSpectroscopyResult]]](Map.empty).map { cache =>
      new ItcClient[F](uri, cache)
    }

}
// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.data.EitherT
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.effect.{Async, Ref, Resource}
import clue.TransactionalClient
import clue.http4sjdk.Http4sJDKBackend
import io.circe.syntax._
import lucuma.core.model.{Observation, Target}
import lucuma.odb.api.model.ObservationModel
import lucuma.odb.api.repo.OdbRepo
import org.http4s.Uri
import org.typelevel.log4cats.Logger

class ItcClient[F[_]: Async: Logger](
  uri:   Uri,
  // For now, we'll just cache results forever (i.e., until the next restart)
  cache: Ref[F, Map[ItcSpectroscopyInput, ItcResult]]
) {

  val resource: Resource[F, TransactionalClient[F, Unit]] =
    for {
      b <- Http4sJDKBackend[F]
      c <- Resource.eval(TransactionalClient.of[F, Unit](uri)(Async[F], b, Logger[F]))
    } yield c

  private def queryOne(
    o:        ObservationModel,
    t:        Target,
    useCache: Boolean
  ): F[ItcResult] = {

    def callItc(in: ItcSpectroscopyInput): F[ItcResult] =
      for {
        x <- resource.use(_.request(ItcQuery)(in))
        r  = x.headOption.map(_.itc).getOrElse(ItcResult.error(s"No ITC result was returned for ${o.id}"))
        _ <- cache.update(_ + (in -> r))
      } yield r

    val input: Option[ItcSpectroscopyInput] =
      ItcSpectroscopyInput.fromObservation(o, t)

    for {
      _    <- Logger[F].info(s"ITC Input:\n${input.asJson.spaces2}")
      cval <- if (useCache) input.flatTraverse { in => cache.get.map(_.get(in)) } else Async[F].pure(None)
      res  <- input.fold(Async[F].pure(ItcResult.error(s"Could not extract required ITC query input from ${o.id}"))) { in =>
        cval.fold(callItc(in))(Async[F].pure)
      }
      _    <- Logger[F].info(s"ITC Result (${cval.fold("from ITC")(_ => "from cache")}):\n$res")
    } yield res
  }

  def query(
    oid:            Observation.Id,
    odb:            OdbRepo[F],
    includeDeleted: Boolean = false,
    useCache:       Boolean = true
  ): F[Either[ItcResult.Error, (Target, ItcResult.Success)]] = {

    val observation: EitherT[F, ItcResult.Error, ObservationModel] =
      EitherT(
        odb
          .observation
          .select(oid, includeDeleted)
          .map(_.toRight(ItcResult.Error(s"Observation $oid not found")))
      )

    val targets: EitherT[F, ItcResult.Error, List[Target]] =
      observation.flatMap { o =>
        EitherT(
          o.targetEnvironment.asterism.toList.flatTraverse {
            tid => odb.target.selectTarget(tid, includeDeleted).map(_.map(_.target).toList)
          }.map {
            case Nil => ItcResult.Error(s"Observation $oid has no targets").asLeft
            case ts  => ts.asRight
          }
        )
      }

    (for {
      o  <- observation
      ts <- targets
      rs <- EitherT(ts.traverse(t => queryOne(o, t, useCache)).map(_.traverse(_.toEither)))
      mx  = ts.zip(rs).maxByOption { case (_, r) => r.exposureTime }
      r  <- EitherT(Async[F].pure(mx.toRight(ItcResult.Error(s"No results returned by ITC for observation $oid"))))
    } yield r).value

  }

}

object ItcClient {

  def create[F[_]: Async: Logger](
    uri: Uri
  ): F[ItcClient[F]] =
    Ref.of[F, Map[ItcSpectroscopyInput, ItcResult]](Map.empty).map { cache =>
      new ItcClient[F](uri, cache)
    }

}
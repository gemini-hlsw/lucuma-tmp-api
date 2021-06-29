// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.client

import lucuma.core.model.{GuestUser, User}

import cats.effect.Sync
import org.http4s.client.Client
import org.http4s.{Request, Uri}
import org.http4s.headers.Authorization

import scala.concurrent.duration._

/**
 * Placeholder until the real SsoClient is on CE3.
 */
trait SsoClient[F[_], A] {

  def find(req: Request[F]): F[Option[A]]

  def get(authorization: Authorization): F[Option[A]]

  def map[B](f: A => B): SsoClient[F, B]

}

object SsoClient {

  final case class UserInfo(user: User, claim: SsoJwtClaim, header: Authorization)

  final case class SsoClientPlaceholder[F[_]: Sync, A](
    a: Option[A]
  ) extends SsoClient[F, A] {
    def find(req: Request[F]): F[Option[A]] = Sync[F].delay(a)
    def get(authorization: Authorization): F[Option[A]] = Sync[F].delay(a)
    def map[B](f: A => B): SsoClient[F, B] = SsoClientPlaceholder(a.map(f))
  }

  def initial[F[_]: Sync](
    httpClient:  Client[F],
    ssoRoot:     Uri,
    jwtReader:   SsoJwtReader[F],
    serviceJwt:  String,
    gracePeriod: FiniteDuration = 5.minutes,
  ): F[SsoClient[F, UserInfo]] =
      Sync[F].delay {

        // "use" the parameters
        println(s"httpClient  = $httpClient")
        println(s"ssoRoot     = $ssoRoot")
        println(s"jwtReader   = $jwtReader")
        println(s"serviceJwt  = $serviceJwt")
        println(s"gracePeriod = $gracePeriod")

        SsoClientPlaceholder(
          User.Id.fromLong(1).map { id =>
            UserInfo(GuestUser(id), null, null)
          }
        )
      }
}

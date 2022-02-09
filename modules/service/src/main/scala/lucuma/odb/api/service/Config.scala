// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.service

// TODO: SSO
//import lucuma.sso.client.SsoClient.UserInfo
//import lucuma.sso.client.{SsoClient, SsoJwtReader}
//import lucuma.sso.client.util.{GpgPublicKeyReader, JwtDecoder}

import cats.effect.{Async, Resource}
import cats.implicits.catsSyntaxTuple2Parallel
import ciris.{ConfigDecoder, ConfigValue, env, prop}
import org.http4s.Uri
import org.http4s.blaze.client.BlazeClientBuilder
import org.http4s.client.Client

// TODO: SSO
//import java.security.PublicKey

/**
 * Application configuration.
 */
final case class Config(
  port:         Int,
  itc:          Uri
//  ssoRoot:      Uri,
//  ssoPublicKey: PublicKey,
//  serviceJwt:   String
) {

// TODO: SSO
//  def jwtReader[F[_]: Concurrent]: SsoJwtReader[F] =
//    SsoJwtReader(JwtDecoder.withPublicKey(ssoPublicKey))

  def httpClientResource[F[_]: Async]: Resource[F, Client[F]] =
    BlazeClientBuilder[F].resource

  // TODO: SSO
  // SSO Client resource (has to be a resource because it owns an HTTP client).
//  def ssoClient[F[_]: Async]: Resource[F, SsoClient[F, UserInfo]] =
//    httpClientResource[F].evalMap { httpClient =>
//      SsoClient.initial(
//        serviceJwt = serviceJwt,
//        ssoRoot    = ssoRoot,
//        jwtReader  = jwtReader[F],
//        httpClient = httpClient,
//      )
//    }
}

object Config {

  implicit val uri: ConfigDecoder[String, Uri] =
    ConfigDecoder[String].mapOption("URI") { s =>
      Uri.fromString(s).toOption
    }

// TODO: SSO
//  implicit val publicKey: ConfigDecoder[String, PublicKey] =
//    ConfigDecoder[String].mapOption("Public Key") { s =>
//      GpgPublicKeyReader.publicKey(s).toOption
//    }

  def envOrProp[F[_]](name: String): ConfigValue[F, String] =
    env(name) or prop(name)

  def fromCiris[F[_]]: ConfigValue[F, Config] =
    (
      (envOrProp[F]("ODB_PORT") or envOrProp[F]("PORT") or ConfigValue.default("8080")).as[Int],
      (envOrProp[F]("ITC_URI") or ConfigValue.default("https://itc-production.herokuapp.com/itc")).as[Uri]
    ).parMapN(Config.apply)

// TODO: SSO
//    envOrProp("ODB_SSO_ROOT").as[Uri],
//    envOrProp("ODB_SSO_PUBLIC_KEY").as[PublicKey],
//    envOrProp("ODB_SERVICE_JWT")
//  ).parMapN(Config.apply)

}
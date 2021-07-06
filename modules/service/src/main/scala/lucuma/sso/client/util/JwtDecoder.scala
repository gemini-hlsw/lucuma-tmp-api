// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.client.util

import cats.MonadError

import java.security.PublicKey

/**
 * Placeholder
 */
trait JwtDecoder[F[_]] {

}

object JwtDecoder {

  def withPublicKey[F[_]](pub: PublicKey)(
    implicit ev: MonadError[F, Throwable]
  ): JwtDecoder[F] =
    new JwtDecoder[F] {

      println(s"pub = $pub, ev = $ev")

    }

}

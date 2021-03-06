// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.client

import lucuma.sso.client.util.JwtDecoder

/**
 * Placeholder
 */
trait SsoJwtReader[F[_]] {

}

object SsoJwtReader {

  def apply[F[_]](jwtDecoder: JwtDecoder[F]): SsoJwtReader[F] =
    new SsoJwtReader[F] {

      // "use" the parameter
      println(s"jwtDecoder = $jwtDecoder")

    }
}

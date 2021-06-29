// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.client.util

import java.security.PublicKey
import cats.syntax.all._

/**
 * Placeholder
 */
object GpgPublicKeyReader {

  def publicKey(pgpArmorText: String): Either[Throwable, PublicKey] =
    new PublicKey {
      override def getAlgorithm: String = ""

      override def getFormat: String = ""

      override def getEncoded: Array[Byte] = pgpArmorText.getBytes()
    }.asRight[Throwable]

}

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.syntax.all._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

/**
 * Input objects for sharing.
 */
final case class Sharing[A, B](
  one:  A,
  many: List[B]
) {

  def tupleLeft: List[(A, B)] =
    many.tupleLeft(one)

  def tupleRight: List[(B, A)] =
    many.tupleRight(one)
}

object Sharing {

  implicit def DecoderSharing[A: Decoder, B: Decoder]: Decoder[Sharing[A, B]] =
    deriveDecoder[Sharing[A, B]]

  implicit def EqSharing[A: Eq, B: Eq]: Eq[Sharing[A, B]] =
    Eq.by { s => (
      s.one,
      s.many
    )}

}

// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.syntax.all._
import io.circe.{Decoder, HCursor}

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

  def customDecoder[A: Decoder, B: Decoder](aName: String, bName: String): Decoder[Sharing[A, B]] =
    (c: HCursor) =>
      for {
        a  <- c.downField(aName).as[A]
        bs <- c.downField(bName).as[List[B]]
      } yield Sharing(a, bs)

  implicit def EqSharing[A: Eq, B: Eq]: Eq[Sharing[A, B]] =
    Eq.by { s => (
      s.one,
      s.many
    )}

}

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.query

import cats.Eq
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder


final case class WhereOptionEq[A: Eq](
  IS_NULL: Option[Boolean],
  EQ:      Option[A],
  NEQ:     Option[A],
  IN:      Option[List[A]],
  NIN:     Option[List[A]],
) extends WhereOption[A] {

  override def allEmpty: Boolean =
    EQ.isEmpty     &&
      NEQ.isEmpty  &&
      IN.isEmpty   &&
      NIN.isEmpty

  def whenNonEmpty: WherePredicate[A] =
    WhereEq(EQ, NEQ, IN, NIN)

  override def matches(a: Option[A]): Boolean =
    optionMatches(a)
}

object WhereOptionEq {

  implicit def DecoderWhereOptionEq[A: Decoder: Eq]: Decoder[WhereOptionEq[A]] =
    deriveDecoder[WhereOptionEq[A]]

}

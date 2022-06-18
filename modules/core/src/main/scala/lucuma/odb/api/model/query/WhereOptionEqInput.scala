// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.query

import cats.Eq
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder


final case class WhereOptionEqInput[A: Eq](
  IS_NULL: Option[Boolean] = None,
  EQ:      Option[A]       = None,
  NEQ:     Option[A]       = None,
  IN:      Option[List[A]] = None,
  NIN:     Option[List[A]] = None
) extends WhereOption[A] {

  override def allEmpty: Boolean =
    EQ.isEmpty     &&
      NEQ.isEmpty  &&
      IN.isEmpty   &&
      NIN.isEmpty

  def whenNonEmpty: WherePredicate[A] =
    WhereEqInput(EQ, NEQ, IN, NIN)

  override def matches(a: Option[A]): Boolean =
    optionMatches(a)
}

object WhereOptionEqInput {

  implicit def DecoderWhereOptionEqInput[A: Decoder: Eq]: Decoder[WhereOptionEqInput[A]] =
    deriveDecoder[WhereOptionEqInput[A]]

}

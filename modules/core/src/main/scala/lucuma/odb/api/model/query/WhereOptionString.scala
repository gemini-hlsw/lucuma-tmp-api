// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.query

import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.refined._
import io.circe.generic.semiauto.deriveDecoder

final case class WhereOptionString(
  IS_NULL:    Option[Boolean],
  EQ:         Option[NonEmptyString],
  NEQ:        Option[NonEmptyString],
  IN:         Option[List[NonEmptyString]],
  NIN:        Option[List[NonEmptyString]],
  LIKE:       Option[NonEmptyString],
  NLIKE:      Option[NonEmptyString],
  MATCH_CASE: Boolean = true
) extends WhereOption[String] {

  def whenEmpty: Boolean =
    EQ.isEmpty && IN.forall(_.isEmpty) && NIN.forall(_.nonEmpty) && LIKE.isEmpty

  def whenNonEmpty: WherePredicate[String] =
    WhereString(EQ, NEQ, IN, NIN, LIKE, NLIKE, MATCH_CASE)

  def matchesNonEmptyString(s: Option[NonEmptyString]): Boolean =
    matches(s.map(_.value))

}

object WhereOptionString {

  implicit val DecoderWhereOptionString: Decoder[WhereOptionString] =
    deriveDecoder[WhereOptionString]

}

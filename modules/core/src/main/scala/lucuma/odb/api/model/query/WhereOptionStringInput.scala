// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.query

import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.refined._
import io.circe.generic.semiauto.deriveDecoder

final case class WhereOptionStringInput(
  IS_NULL:    Option[Boolean],
  EQ:         Option[NonEmptyString],
  NEQ:        Option[NonEmptyString],
  IN:         Option[List[NonEmptyString]],
  NIN:        Option[List[NonEmptyString]],
  LIKE:       Option[NonEmptyString],
  NLIKE:      Option[NonEmptyString],
  MATCH_CASE: Boolean = true
) extends WhereOption[String] {

  override def allEmpty: Boolean =
    EQ.isEmpty     &&
      NEQ.isEmpty  &&
      IN.isEmpty   &&
      NIN.isEmpty  &&
      LIKE.isEmpty &&
      NLIKE.isEmpty

  def whenNonEmpty: WherePredicate[String] =
    WhereStringInput(EQ, NEQ, IN, NIN, LIKE, NLIKE, MATCH_CASE)

  def matchesNonEmptyString(s: Option[NonEmptyString]): Boolean =
    matches(s.map(_.value))

  override def matches(a: Option[String]): Boolean =
    optionMatches(a)

}

object WhereOptionStringInput {

  implicit val DecoderWhereOptionStringInput: Decoder[WhereOptionStringInput] =
    deriveDecoder[WhereOptionStringInput]

}

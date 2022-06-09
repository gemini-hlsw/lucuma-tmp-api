// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.query

import cats.syntax.eq._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.refined._
import io.circe.generic.semiauto.deriveDecoder

final case class WhereOptionString(
  eq:        Option[NonEmptyString],
  neq:       Option[NonEmptyString],
  in:        Option[List[NonEmptyString]],
  nin:       Option[List[NonEmptyString]],
  like:      Option[NonEmptyString],
  nlike:     Option[NonEmptyString],
  isNull:    Option[Boolean],
  matchCase: Boolean = true
) extends WherePredicate[Option[String]] {

  def matches(s: Option[String]): Boolean = {

    def whenEmpty: Boolean =
      eq.isEmpty && in.forall(_.isEmpty) && nin.forall(_.nonEmpty) && like.isEmpty

    isNull.forall(_ === s.isEmpty) &&
      s.fold(whenEmpty)(WhereString(eq, neq, in, nin, like, nlike, matchCase).matches)

  }

  def matchesNonEmpty(s: Option[NonEmptyString]): Boolean =
    matches(s.map(_.value))

}

object WhereOptionString {

  implicit val DecoderWhereOptionString: Decoder[WhereOptionString] =
    deriveDecoder[WhereOptionString]

}

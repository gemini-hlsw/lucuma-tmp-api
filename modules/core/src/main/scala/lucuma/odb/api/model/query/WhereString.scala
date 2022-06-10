// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.query

import cats.syntax.eq._
import eu.timepit.refined.types.string.NonEmptyString

import scala.util.matching.Regex

final case class WhereString(
  EQ:         Option[NonEmptyString],
  NEQ:        Option[NonEmptyString],
  IN:         Option[List[NonEmptyString]],
  NIN:        Option[List[NonEmptyString]],
  LIKE:       Option[NonEmptyString],
  NLIKE:      Option[NonEmptyString],
  MATCH_CASE: Boolean = true
) extends WherePredicate[String] {

  private def forMatching(s: NonEmptyString): String =
    if (MATCH_CASE) s.value else s.value.toLowerCase

  private val eqʹ: Option[String] =
    EQ.map(forMatching)

  private val neqʹ: Option[String] =
    NEQ.map(forMatching)

  private val inʹ: Option[List[String]] =
    IN.map(_.map(forMatching))

  private val ninʹ: Option[List[String]] =
    NIN.map(_.map(forMatching))

  // Want to ignore Regex symbols in the input stream and only work on those
  // that we'll be adding.
  private val Symbols: Regex    = "[\\{\\}\\(\\)\\[\\]\\.\\+\\*\\?\\^\\$\\|]".r

  // Match % if not preceded by \
  private val MatchMany: Regex  = "(?<!\\\\)%".r

  // Match _ if not preceded by \
  private val MatchOne: Regex   = "(?<!\\\\)_".r

  // Match \%
  private val EscapeMany: Regex = raw"\\%".r

  // Match \_
  private val EscapeOne: Regex  = raw"\\_".r

  def wildcardToRegex(wild: NonEmptyString): Regex = {
    val wildʹ = forMatching(wild)
    val regex =
      EscapeOne.replaceAllIn(
        MatchOne.replaceAllIn(
          EscapeMany.replaceAllIn(
            MatchMany.replaceAllIn(Symbols.replaceAllIn(wildʹ, "\\\\$0"), ".*"),
            "%"
          ),
          "."
        ),
        "_"
      )

    (s"^$regex$$").r
  }

  private val likeʹ: Option[Regex] =
    LIKE.map(wildcardToRegex)

  private val nlikeʹ: Option[Regex] =
    NLIKE.map(wildcardToRegex)

  override def matches(s: String): Boolean = {

    val sʹ = if (MATCH_CASE) s else s.toLowerCase

    super.matches(s)               &&
      eqʹ.forall(_ === sʹ)         &&
      neqʹ.forall(_ =!= sʹ)        &&
      inʹ.forall(_.contains(sʹ))   &&
      ninʹ.forall(!_.contains(sʹ)) &&
      likeʹ.forall(_.matches(sʹ))  &&
      nlikeʹ.forall(!_.matches(sʹ))
  }

  def matchesNonEmptyString(s: NonEmptyString): Boolean =
    matches(s.value)

}



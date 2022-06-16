// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.query

import cats.syntax.eq._

trait WhereOption[A] extends WherePredicate[Option[A]] {

  def IS_NULL: Option[Boolean]

  def allEmpty: Boolean
  def whenNonEmpty: WherePredicate[A]

  def optionMatches(a: Option[A]): Boolean =
    IS_NULL.forall(_ === a.isEmpty)          &&
      a.fold(allEmpty)(whenNonEmpty.matches)

}

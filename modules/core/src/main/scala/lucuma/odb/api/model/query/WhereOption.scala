// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.query

import cats.syntax.eq._

trait WhereOption[A] extends WherePredicate[Option[A]] {

  def IS_NULL: Option[Boolean]

  def whenEmpty: Boolean
  def whenNonEmpty: WherePredicate[A]

  override def matches(a: Option[A]): Boolean =
    super.matches(a)                           &&
      IS_NULL.forall(_ === a.isEmpty)          &&
      a.fold(whenEmpty)(whenNonEmpty.matches)

}

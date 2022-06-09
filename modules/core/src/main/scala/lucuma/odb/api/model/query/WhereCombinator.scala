// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.query

/**
 *
 */
trait WhereCombinator[A] extends WherePredicate[A] {

  def and: Option[List[WhereCombinator[A]]]

  def or: Option[List[WhereCombinator[A]]]

  def not: Option[WhereCombinator[A]]

  protected def combinatorMatch(a: A): Boolean =
    and.forall(_.forall(_.matches(a)))  &&
      or.forall(_.exists(_.matches(a))) &&
      not.forall(!_.matches(a))

}

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.query

/**
 *
 */
trait WhereCombinator[A] extends WherePredicate[A] {

  def AND: Option[List[WhereCombinator[A]]]

  def OR: Option[List[WhereCombinator[A]]]

  def NOT: Option[WhereCombinator[A]]

  protected def combinatorMatch(a: A): Boolean =
    AND.forall(_.forall(_.matches(a)))  &&
      OR.forall(_.exists(_.matches(a))) &&
      NOT.forall(!_.matches(a))

}

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.query

trait WhereCombinator[A] extends WherePredicate[A] {

  def AND: Option[List[WherePredicate[A]]]

  def OR: Option[List[WherePredicate[A]]]

  def NOT: Option[WherePredicate[A]]

  def combinatorMatches(a: A): Boolean =
    AND.forall(_.forall(_.matches(a)))  &&
      OR.forall(_.exists(_.matches(a))) &&
      NOT.forall(!_.matches(a))

}

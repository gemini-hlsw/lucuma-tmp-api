// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.query

import cats.Eq


trait SelectResult[A] {
  def matches: List[A]
  def hasMore: Boolean
}

object SelectResult {

  final case class Standard[A](
    matches: List[A],
    hasMore: Boolean,
  ) extends SelectResult[A]

  object Standard {

    implicit def EqStandard[A: Eq]: Eq[Standard[A]] =
      Eq.by { a => (
        a.matches,
        a.hasMore
      )}

  }

}
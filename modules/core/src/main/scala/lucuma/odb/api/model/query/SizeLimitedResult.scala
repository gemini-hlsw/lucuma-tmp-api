// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.query

import cats.Eq
import cats.syntax.order._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.all.NonNegInt

trait SizeLimitedResult[A] {
  def limitedValues: List[A]
  def hasMore:       Boolean
}

object SizeLimitedResult {

  val MaxSize: NonNegInt =
    1000

  def size(explicitLimit: Option[NonNegInt]): NonNegInt =
    explicitLimit.getOrElse(MaxSize) min MaxSize

  final case class Select[A](
    limitedValues: List[A],
    hasMore:       Boolean,
  ) extends SizeLimitedResult[A]

  object Select {

    def fromAll[A](all: List[A], explicitLimit: Option[NonNegInt]): Select[A] = {
      val (m, r) = all.splitAt(size(explicitLimit).value)
      Select(m, r.nonEmpty)
    }

    implicit def EqSelect[A: Eq]: Eq[Select[A]] =
      Eq.by { a => (
        a.limitedValues,
        a.hasMore
      )}

  }

  final case class Update[A](
    allValues:     List[A],
    limitedValues: List[A],
    hasMore:       Boolean
  ) extends SizeLimitedResult[A]

  object Update {

    def fromAll[A](all: List[A], explicitLimit: Option[NonNegInt]): Update[A] = {
      val (m, r) = all.splitAt(size(explicitLimit).value)
      Update(all, m, r.nonEmpty)
    }

    implicit def EqUpdate[A: Eq]: Eq[Update[A]] =
      Eq.by { a => (
        a.allValues,
        a.limitedValues,
        a.hasMore
      )}

  }

}
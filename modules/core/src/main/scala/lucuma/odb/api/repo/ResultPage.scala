// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.Eq

final case class ResultPage[A](
  nodes:       List[A],
  hasNextPage: Boolean
)

object ResultPage {

  def empty[A]: ResultPage[A] =
    ResultPage(Nil, hasNextPage = false)

  def EqResultPage[A: Eq]: Eq[ResultPage[A]] =
    Eq.by { p => (
      p.nodes,
      p.hasNextPage
    )}

}

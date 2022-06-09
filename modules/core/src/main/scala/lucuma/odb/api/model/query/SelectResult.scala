// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.query

import eu.timepit.refined.types.all.NonNegInt

trait SelectResult[A] {
  def matches:    List[A]
  def totalCount: NonNegInt
  def hasMore:    Boolean
}

object SelectResult {

  final case class Standard[A](
    matches:    List[A],
    totalCount: NonNegInt
  ) extends SelectResult[A] {

    override def hasMore: Boolean =
      matches.size < totalCount.value

  }

}
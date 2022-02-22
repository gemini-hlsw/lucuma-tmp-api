// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.kernel.BoundedEnumerable
import cats.kernel.Order.catsKernelOrderingForOrder
import lucuma.core.util.Gid
import monocle.{Focus, Lens}

import scala.collection.immutable.SortedMap

final case class Table[K, V](
  lastKey: K,
  rows:    SortedMap[K, V]
)

object Table {

  def empty[K: Gid, V]: Table[K, V] =
    Table(BoundedEnumerable[K].minBound, SortedMap.empty)

  implicit def EqTable[K: Eq, V: Eq]: Eq[Table[K, V]] =
    Eq.by { a => (a.lastKey, a.rows) }

  def lastKey[K, V]: Lens[Table[K, V], K] =
    Focus.apply(_.lastKey)

  def rows[K, V]: Lens[Table[K, V], SortedMap[K, V]] =
    Focus.apply(_.rows)

}

// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.Order
import cats.implicits.catsKernelOrderingForOrder
import lucuma.core.model.Target

import scala.collection.immutable.SortedSet

/**
 * `CommonTarget` expresses what is the same across of collection of
 * `TargetModel` (the core model `Target` itself) along with a reference
 * to the ids of individual `TargetModel`s with this `Target`.
 */
final case class CommonTarget(
  target: Target,
  ids:    SortedSet[Target.Id]
) extends TargetHolder

object CommonTarget {

  implicit val OrderCommonTarget: Order[CommonTarget] =
    Order.from { (a, b) =>
      Target.TargetNameOrder.compare(a.target, b.target) match {
        case 0 => Order[SortedSet[Target.Id]].compare(a.ids, b.ids)
        case i => i
      }
    }

  def from(target: Target, ids: IterableOnce[Target.Id]): CommonTarget =
    CommonTarget(target, SortedSet.from(ids))

  /**
   * Finds the `CommonTarget`s in a list of `TargetModel` by grouping on their
   * `lucuma.core.model.Target`s.
   */
  def extractFromTargetModels(tms: List[TargetModel]): SortedSet[CommonTarget] =
    SortedSet.from(tms.groupMap(_.target)(_.id).map((from _).tupled))

}


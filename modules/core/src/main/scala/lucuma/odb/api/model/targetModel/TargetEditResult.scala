// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.Eq
import lucuma.core.util.Enumerated


/**
 * An ADT describing which target edits took place in a bulk edit.
 */
final case class TargetEditResult(
  op:     TargetEditResult.Op,
  target: TargetModel
)

object TargetEditResult {

  sealed trait Op extends Product with Serializable

  object Op {

    case object Create extends Op
    case object Edit   extends Op
    case object Delete extends Op

    implicit val EnumeratedOp: Enumerated[Op] =
      Enumerated.of(Create, Edit, Delete)

  }

  implicit val EqTargetEditDescDesc: Eq[TargetEditResult] =
    Eq.by { a => (
      a.op,
      a.target
    )}

  def create(t: TargetModel): TargetEditResult =
    TargetEditResult(Op.Create, t)

  def edit(t: TargetModel): TargetEditResult =
    TargetEditResult(Op.Edit, t)

  def delete(t: TargetModel): TargetEditResult =
    TargetEditResult(Op.Delete, t)

}
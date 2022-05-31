// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.syntax

import cats.syntax.option._
import lucuma.core.math.Angle
import lucuma.core.model.{PosAngle => PosAngleConstraint}
import monocle.Optional

// TODO: Delete.  All of this is in lucuma core version 40 or better.

final class PosAngleConstraintCompanionOps(val self: PosAngleConstraint.type) extends AnyVal {

  def fixed(a: Angle): PosAngleConstraint =
    PosAngleConstraint.Fixed(a)

  def allowFlip(a: Angle): PosAngleConstraint =
    PosAngleConstraint.AllowFlip(a)

  def averageParallactic: PosAngleConstraint =
    PosAngleConstraint.AverageParallactic

  def parallacticOverride(a: Angle): PosAngleConstraint =
    PosAngleConstraint.ParallacticOverride(a)

  def unconstrained: PosAngleConstraint =
    PosAngleConstraint.Unconstrained

  def angle: Optional[PosAngleConstraint, Angle] =
    Optional[PosAngleConstraint, Angle]({
      case PosAngleConstraint.Fixed(angle)               => angle.some
      case PosAngleConstraint.AllowFlip(angle)           => angle.some
      case PosAngleConstraint.AverageParallactic         => none
      case PosAngleConstraint.ParallacticOverride(angle) => angle.some
      case PosAngleConstraint.Unconstrained              => none
    })({ a => {
      case PosAngleConstraint.Fixed(_)                   => PosAngleConstraint.Fixed(a)
      case PosAngleConstraint.AllowFlip(_)               => PosAngleConstraint.AllowFlip(a)
      case PosAngleConstraint.AverageParallactic         => PosAngleConstraint.AverageParallactic
      case PosAngleConstraint.ParallacticOverride(_)     => PosAngleConstraint.ParallacticOverride(a)
      case PosAngleConstraint.Unconstrained              => PosAngleConstraint.Unconstrained
    }})

}

trait ToPosAngleConstraintCompanionOps {
  implicit def toPosAngleConstraintCompanionOps(self: PosAngleConstraint.type): PosAngleConstraintCompanionOps =
    new PosAngleConstraintCompanionOps(self)
}

object posangleconstraint extends ToPosAngleConstraintCompanionOps
// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.syntax.eq._
import cats.syntax.functor._
import cats.syntax.option._
import eu.timepit.refined.types.all.NonEmptyString
import eu.timepit.refined.auto._
import lucuma.core.math.Angle
import lucuma.core.model.{PosAngle => CorePosAngleConstraint}
import lucuma.core.util.Enumerated
import lucuma.odb.api.model.syntax.posangleconstraint._
import monocle.{Focus, Iso, Lens, Prism}
import monocle.macros.GenPrism


sealed trait PosAngleConstraint extends Product with Serializable {

  def constraintType: PosAngleConstraint.Type

  def toCorePosAngleConstraint: CorePosAngleConstraint

}

object PosAngleConstraint {

  sealed abstract class Type(
    val tag: NonEmptyString
  ) extends Product with Serializable

  object Type {
    case object Fixed               extends Type("Fixed")
    case object AllowFlip           extends Type("AllowFlip")
    case object AverageParallactic  extends Type("AverageParallactic")
    case object ParallacticOverride extends Type("ParallacticOverride")

    implicit val EnumeratedType: Enumerated[Type] =
      Enumerated.of(
        Fixed,
        AllowFlip,
        AverageParallactic,
        ParallacticOverride
      )
  }

  final case class Fixed(
    angle:     Angle,
    allowFlip: Boolean
  ) extends PosAngleConstraint {

    def constraintType: PosAngleConstraint.Type =
      if (allowFlip) Type.AllowFlip else Type.Fixed

    def toCorePosAngleConstraint: CorePosAngleConstraint =
      if (allowFlip) CorePosAngleConstraint.fixed(angle)
      else CorePosAngleConstraint.allowFlip(angle)

  }

  object Fixed {

    implicit val EqFixed: Eq[Fixed] =
      Eq.by { a => (
        a.angle,
        a.angle
      )}

    val angle: Lens[Fixed, Angle] =
      Focus[Fixed](_.angle)

    val allowFlip: Lens[Fixed, Boolean] =
      Focus[Fixed](_.allowFlip)

  }

  final case class AverageParallactic(
    overrideAngle: Option[Angle]
  ) extends PosAngleConstraint {

    def constraintType: PosAngleConstraint.Type =
      overrideAngle.as(Type.ParallacticOverride).getOrElse(Type.AverageParallactic)

    override def toCorePosAngleConstraint: CorePosAngleConstraint =
      overrideAngle.fold(CorePosAngleConstraint.averageParallactic)(CorePosAngleConstraint.parallacticOverride)

  }

  object AverageParallactic {

    implicit val EqAverageParallactic: Eq[AverageParallactic] =
      Eq.by(_.overrideAngle)

    val overrideAngle: Lens[AverageParallactic, Option[Angle]] =
      Focus[AverageParallactic](_.overrideAngle)
  }

  val fixed: Prism[PosAngleConstraint, Fixed] =
    GenPrism[PosAngleConstraint, Fixed]

  val averageParallactic: Prism[PosAngleConstraint, AverageParallactic] =
    GenPrism[PosAngleConstraint, AverageParallactic]

  val corePosAngleConstraint: Iso[Option[PosAngleConstraint], CorePosAngleConstraint] =
    Iso.apply[Option[PosAngleConstraint], CorePosAngleConstraint] {
      case None                            => CorePosAngleConstraint.unconstrained
      case Some(f @ Fixed(_, _))           => f.toCorePosAngleConstraint
      case Some(a @ AverageParallactic(_)) => a.toCorePosAngleConstraint
    } {
      case CorePosAngleConstraint.Fixed(a)               => (Fixed(a, allowFlip = false): PosAngleConstraint).some
      case CorePosAngleConstraint.AllowFlip(a)           => (Fixed(a, allowFlip = true): PosAngleConstraint).some
      case CorePosAngleConstraint.AverageParallactic     => (AverageParallactic(none): PosAngleConstraint).some
      case CorePosAngleConstraint.ParallacticOverride(a) => (AverageParallactic(a.some): PosAngleConstraint).some
      case CorePosAngleConstraint.Unconstrained          => none
    }

  implicit val EqPosAngleConstraint: Eq[PosAngleConstraint] =
    Eq.instance {
      case (Fixed(a0, f0),          Fixed(a1, f1)         ) => (a0 === a1) && (f0 === f1)
      case (AverageParallactic(a0), AverageParallactic(a1)) => a0 === a1
      case _                                                => false
    }
}


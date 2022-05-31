// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import eu.timepit.refined.types.all.NonEmptyString
import eu.timepit.refined.auto._
import lucuma.core.math.Angle
import lucuma.core.util.Enumerated
import monocle.{Focus, Lens}


final case class PosAngleConstraint(
  constraint: PosAngleConstraint.Type,
  angle:      Option[Angle]
)

object PosAngleConstraint {

  sealed abstract class Type(
    val tag: NonEmptyString
  ) extends Product with Serializable

  object Type {
    case object Fixed              extends Type("Fixed")
    case object AllowFlip          extends Type("AllowFlip")
    case object AverageParallactic extends Type("AverageParallactic")

    implicit val EnumeratedType: Enumerated[Type] =
      Enumerated.of[Type](
        Fixed,
        AllowFlip,
        AverageParallactic
      )
  }

  implicit val EqPosAngleConstraint: Eq[PosAngleConstraint] =
    Eq.by(a => (
      a.constraint,
      a.angle,
    ))

  val constraint: Lens[PosAngleConstraint, Type] =
    Focus[PosAngleConstraint](_.constraint)

  val angle: Lens[PosAngleConstraint, Option[Angle]] =
    Focus[PosAngleConstraint](_.angle)

}


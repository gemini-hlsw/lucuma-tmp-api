// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import lucuma.core.`enum`.MagnitudeBand
import lucuma.core.math.{Coordinates, Declination, Epoch, Parallax, ProperMotion, RadialVelocity, RightAscension}
import lucuma.core.model.{CatalogId, Magnitude, SiderealTracking, Target}
import cats.Eq
import monocle.{Focus, Lens, Optional}

import scala.collection.immutable.SortedMap

/**
 * TargetModel pairs an id with a `lucuma.core.model.Target` and tracks the
 * target environment in which the target is found.
 */
final case class TargetModel(
  id:                  Target.Id,
  targetEnvironmentId: TargetEnvironment.Id,
  target:              Target
) extends TargetHolder

object TargetModel extends TargetModelOptics {

  implicit val EqTargetModel: Eq[TargetModel] =
    Eq.by { a =>
      (
        a.id,
        a.targetEnvironmentId,
        a.target
      )
    }

}

trait TargetModelOptics { self: TargetModel.type =>

  val target: Lens[TargetModel, Target] =
    Focus[TargetModel](_.target)

  val catalogId: Optional[Target, Option[CatalogId]] =
    Target.siderealTracking.andThen(SiderealTracking.catalogId)

  val coordinates: Optional[Target, Coordinates] =
    Target.siderealTracking.andThen(SiderealTracking.baseCoordinates)

  val ra: Optional[Target, RightAscension] =
    coordinates.andThen(Coordinates.rightAscension)

  val dec: Optional[Target, Declination] =
    coordinates.andThen(Coordinates.declination)

  val epoch: Optional[Target, Epoch] =
    Target.siderealTracking.andThen(SiderealTracking.epoch)

  val properMotion: Optional[Target, Option[ProperMotion]] =
    Target.siderealTracking.andThen(SiderealTracking.properMotion)

  val radialVelocity: Optional[Target, Option[RadialVelocity]] =
    Target.siderealTracking.andThen(SiderealTracking.radialVelocity)

  val parallax: Optional[Target, Option[Parallax]] =
    Target.siderealTracking.andThen(SiderealTracking.parallax)

  val magnitudes: Lens[Target, SortedMap[MagnitudeBand, Magnitude]] =
    Target.magnitudes

}

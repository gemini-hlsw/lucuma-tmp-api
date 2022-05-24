// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import RadialVelocityModel.Input

import lucuma.core.math.RadialVelocity
import lucuma.core.math.arb.ArbRadialVelocity
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary


trait ArbRadialVelocityModel {

  import ArbRadialVelocity._

  private[this] val cmPerSec: Gen[Long] =
    arbitrary[RadialVelocity].map(_.rv.value.underlying.movePointRight(3).longValue)

  private[this] val mPerSec: Gen[BigDecimal] =
    arbitrary[RadialVelocity].map(_.rv.value)

  private[this] val kmPerSec: Gen[BigDecimal] =
    arbitrary[RadialVelocity].map(RadialVelocity.kilometerspersecond.reverseGet)

  implicit val arbRadialVelocityModelInput: Arbitrary[RadialVelocityModel.Input] =
    Arbitrary {
      Gen.oneOf(
        cmPerSec.map(Input.fromCentimetersPerSecond),
        mPerSec.map(Input.fromMetersPerSecond),
        kmPerSec.map(Input.fromKilometersPerSecond)
      )
    }

  implicit val cogRadialVelocityModelInput: Cogen[RadialVelocityModel.Input] =
    Cogen[(
      Option[Long],
      Option[BigDecimal],
      Option[BigDecimal]
    )].contramap { in =>
      (in.centimetersPerSecond, in.metersPerSecond, in.kilometersPerSecond)
    }
}

object ArbRadialVelocityModel extends ArbRadialVelocityModel

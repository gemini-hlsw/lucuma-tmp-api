// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import DeclinationModel.Input

import lucuma.core.math.Declination
import lucuma.core.math.arb.ArbDeclination

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbDeclinationModel {
  import ArbDeclination._

  private[this] val microarcseconds: Gen[Long] =
    arbitrary[Declination].map(_.toAngle.toMicroarcseconds)

  private[this] val degrees: Gen[BigDecimal] =
    arbitrary[Declination].map(d => BigDecimal(d.toAngle.toDoubleDegrees))

  implicit val arbDeclinationModelInput: Arbitrary[DeclinationModel.Input] =
    Arbitrary {
      Gen.oneOf(
        microarcseconds.map(Input.fromMicroarcseconds),
        degrees.map(Input.fromDegrees),
        arbitrary[Declination].map(Input.fromDms)
      )
    }

  implicit val cogDeclinationModelInput: Cogen[DeclinationModel.Input] =
    Cogen[(
      Option[Long],
      Option[BigDecimal],
      Option[Declination]
    )].contramap { in =>
      (in.microarcseconds, in.degrees, in.dms)
    }
}

object ArbDeclinationModel extends ArbDeclinationModel


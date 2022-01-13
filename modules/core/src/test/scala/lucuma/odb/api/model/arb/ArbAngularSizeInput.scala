// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.odb.api.model.targetModel.AngularSizeInput
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary


trait ArbAngularSizeInput {

  import ArbAngleModel._

  implicit val arbAngularSizeInput: Arbitrary[AngularSizeInput] =
    Arbitrary {
      for {
        mj <- arbitrary[Option[AngleModel.AngleInput]]
        mn <- arbitrary[Option[AngleModel.AngleInput]]
      } yield AngularSizeInput(mj, mn)
    }

  implicit val cogAngularSizeInput: Cogen[AngularSizeInput] =
    Cogen[(
      Option[AngleModel.AngleInput],
      Option[AngleModel.AngleInput]
    )].contramap { a => (
      a.majorAxis,
      a.minorAxis
    )}

}

object ArbAngularSizeInput extends ArbAngularSizeInput

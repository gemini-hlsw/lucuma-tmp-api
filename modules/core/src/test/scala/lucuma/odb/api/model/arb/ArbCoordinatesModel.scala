// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbCoordinatesModel {

  import ArbDeclinationModel._
  import ArbRightAscensionModel._

  implicit val arbCoordinatesModelInput: Arbitrary[CoordinatesModel.Input] =
    Arbitrary {
      for {
        r <- arbitrary[RightAscensionModel.Input]
        d <- arbitrary[DeclinationModel.Input]
      } yield CoordinatesModel.Input(r, d)
    }

  implicit val cogCoordinatesModelInput: Cogen[CoordinatesModel.Input] =
    Cogen[(
      RightAscensionModel.Input,
      DeclinationModel.Input
    )].contramap { in =>
      (in.ra, in.dec)
    }
}

object ArbCoordinatesModel extends ArbCoordinatesModel

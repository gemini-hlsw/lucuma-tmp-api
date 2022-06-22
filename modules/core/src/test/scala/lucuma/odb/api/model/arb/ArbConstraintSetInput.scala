// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import clue.data.Input
import eu.timepit.refined.scalacheck._
import lucuma.core.enums._
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbConstraintSetInput {
  import ArbElevationRangeInput._
  import ArbEnumerated._
  import ArbInput._

  implicit val arbConstraintSetInput: Arbitrary[ConstraintSetInput] =
    Arbitrary {
      for {
        iq   <- arbitrary[Input[ImageQuality]]
        ce   <- arbitrary[Input[CloudExtinction]]
        sb   <- arbitrary[Input[SkyBackground]]
        wv   <- arbitrary[Input[WaterVapor]]
        erc  <- arbitrary[Input[ElevationRangeInput]]
      } yield ConstraintSetInput(iq, ce, sb, wv, erc)
    }

  implicit val cogConstraintSetInput: Cogen[ConstraintSetInput] =
    Cogen[
      (Input[ImageQuality],
       Input[CloudExtinction],
       Input[SkyBackground],
       Input[WaterVapor],
       Input[ElevationRangeInput]
      )
    ].contramap(cs =>
      (cs.imageQuality,
       cs.cloudExtinction,
       cs.skyBackground,
       cs.waterVapor,
       cs.elevationRange
      )
    )
}

object ArbConstraintSetInput extends ArbConstraintSetInput

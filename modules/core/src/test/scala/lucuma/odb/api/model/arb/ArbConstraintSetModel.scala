// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import clue.data.Input
import eu.timepit.refined.scalacheck._
import lucuma.core.enum._
import lucuma.core.util.arb.{ ArbEnumerated, ArbGid }
import lucuma.odb.api.model.{ ConstraintSetModel, ElevationRangeModel }
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbConstraintSetModel {
  import ArbElevationRange._
  import ArbEnumerated._
  import ArbGid._
  import ArbInput._

  implicit val arbConstraintSet: Arbitrary[ConstraintSetModel] =
    Arbitrary {
      for {
        iq   <- arbitrary[ImageQuality]
        ce   <- arbitrary[CloudExtinction]
        sb   <- arbitrary[SkyBackground]
        wv   <- arbitrary[WaterVapor]
        er   <- arbitrary[ElevationRangeModel]
      } yield ConstraintSetModel(iq, ce, sb, wv, er)
    }

  implicit val cogConstraintSet: Cogen[ConstraintSetModel] =
    Cogen[
      (ImageQuality,
       CloudExtinction,
       SkyBackground,
       WaterVapor,
       ElevationRangeModel
      )
    ].contramap(cs =>
      (cs.imageQuality,
       cs.cloudExtinction,
       cs.skyBackground,
       cs.waterVapor,
       cs.elevationRange
      )
    )

  implicit val arbConstraintSetCreate: Arbitrary[ConstraintSetModel.Create] =
    Arbitrary {
      for {
        iq   <- arbitrary[ImageQuality]
        ce   <- arbitrary[CloudExtinction]
        sb   <- arbitrary[SkyBackground]
        wv   <- arbitrary[WaterVapor]
        erc  <- arbitrary[ElevationRangeModel.Create]
      } yield ConstraintSetModel.Create(iq, ce, sb, wv, erc)
    }

  implicit val cogConstraintSetCreate: Cogen[ConstraintSetModel.Create] =
    Cogen[
      (ImageQuality,
       CloudExtinction,
       SkyBackground,
       WaterVapor,
       ElevationRangeModel.Create
      )
    ].contramap(cs =>
      (cs.imageQuality,
       cs.cloudExtinction,
       cs.skyBackground,
       cs.waterVapor,
       cs.elevationRange
      )
    )

  implicit val arbConstraintSetEdit: Arbitrary[ConstraintSetModel.Edit] =
    Arbitrary {
      for {
        iq   <- arbitrary[Input[ImageQuality]]
        c    <- arbitrary[Input[CloudExtinction]]
        sb   <- arbitrary[Input[SkyBackground]]
        wv   <- arbitrary[Input[WaterVapor]]
        er   <- arbitrary[Input[ElevationRangeModel.Create]]
      } yield ConstraintSetModel.Edit(iq, c, sb, wv, er)
    }

  implicit val cogConstraintSetEdit: Cogen[ConstraintSetModel.Edit] =
    Cogen[
      (Input[ImageQuality],
       Input[CloudExtinction],
       Input[SkyBackground],
       Input[WaterVapor],
       Input[ElevationRangeModel.Create]
      )
    ].contramap(cse =>
      (cse.imageQuality,
       cse.cloudExtinction,
       cse.skyBackground,
       cse.waterVapor,
       cse.elevationRange
      )
    )
}

object ArbConstraintSetModel extends ArbConstraintSetModel

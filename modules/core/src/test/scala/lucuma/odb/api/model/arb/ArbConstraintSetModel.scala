// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import clue.data.Input
import eu.timepit.refined.scalacheck._
import eu.timepit.refined.scalacheck.string.nonEmptyStringArbitrary
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enum._
import lucuma.core.model.{ ConstraintSet, Program }
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
        id   <- arbitrary[ConstraintSet.Id]
        ex   <- arbitrary[Existence]
        pid  <- arbitrary[Program.Id]
        name <- arbitrary[NonEmptyString]
        iq   <- arbitrary[ImageQuality]
        ce   <- arbitrary[CloudExtinction]
        sb   <- arbitrary[SkyBackground]
        wv   <- arbitrary[WaterVapor]
        er   <- arbitrary[ElevationRangeModel]
      } yield ConstraintSetModel(id, ex, pid, name, iq, ce, sb, wv, er)
    }

  implicit val cogConstraintSet: Cogen[ConstraintSetModel] =
    Cogen[
      (
        ConstraintSet.Id,
        Existence,
        Program.Id,
        NonEmptyString,
        ImageQuality,
        CloudExtinction,
        SkyBackground,
        WaterVapor,
        ElevationRangeModel
      )
    ].contramap(cs =>
      (cs.id,
       cs.existence,
       cs.programId,
       cs.name,
       cs.imageQuality,
       cs.cloudExtinction,
       cs.skyBackground,
       cs.waterVapor,
       cs.elevationRange
      )
    )

  implicit val arbConstraintSetCreate: Arbitrary[ConstraintSetModel.Create] =
    Arbitrary {
      for {
        id   <- arbitrary[Option[ConstraintSet.Id]]
        pid  <- arbitrary[Program.Id]
        name <- arbitrary[String]
        iq   <- arbitrary[ImageQuality]
        ce   <- arbitrary[CloudExtinction]
        sb   <- arbitrary[SkyBackground]
        wv   <- arbitrary[WaterVapor]
        erc  <- arbitrary[ElevationRangeModel.Create]
      } yield ConstraintSetModel.Create(id, pid, name, iq, ce, sb, wv, erc)
    }

  implicit val cogConstraintSetCreate: Cogen[ConstraintSetModel.Create] =
    Cogen[
      (
        Option[ConstraintSet.Id],
        Program.Id,
        String,
        ImageQuality,
        CloudExtinction,
        SkyBackground,
        WaterVapor,
        ElevationRangeModel.Create
      )
    ].contramap(cs =>
      (cs.constraintSetId,
       cs.programId,
       cs.name,
       cs.imageQuality,
       cs.cloudExtinction,
       cs.skyBackground,
       cs.waterVapor,
       cs.elevationRange
      )
    )

  implicit val arbConstraintSetEdit: Arbitrary[ConstraintSetModel.Edit] =
    Arbitrary {
      for {
        csid <- arbitrary[ConstraintSet.Id]
        ex   <- arbitrary[Input[Existence]]
        n    <- arbitrary[Input[String]]
        iq   <- arbitrary[Input[ImageQuality]]
        c    <- arbitrary[Input[CloudExtinction]]
        sb   <- arbitrary[Input[SkyBackground]]
        wv   <- arbitrary[Input[WaterVapor]]
        er   <- arbitrary[Input[ElevationRangeModel.Create]]
      } yield ConstraintSetModel.Edit(csid, ex, n, iq, c, sb, wv, er)
    }

  implicit val cogConstraintSetEdit: Cogen[ConstraintSetModel.Edit] =
    Cogen[
      (
        ConstraintSet.Id,
        Input[Existence],
        Input[String],
        Input[ImageQuality],
        Input[CloudExtinction],
        Input[SkyBackground],
        Input[WaterVapor],
        Input[ElevationRangeModel.Create]
      )
    ].contramap(cse =>
      (cse.constraintSetId,
       cse.existence,
       cse.name,
       cse.imageQuality,
       cse.cloudExtinction,
       cse.skyBackground,
       cse.waterVapor,
       cse.elevationRange
      )
    )
}

object ArbConstraintSetModel extends ArbConstraintSetModel

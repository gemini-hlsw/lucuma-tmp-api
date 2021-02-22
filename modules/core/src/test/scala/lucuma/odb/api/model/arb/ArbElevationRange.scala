// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.arb

import eu.timepit.refined.scalacheck._
import eu.timepit.refined.scalacheck.numeric.intervalClosedArbitrary
import lucuma.core.util.arb.ArbEnumerated
import lucuma.odb.api.model.{ AirmassRange, ElevationRangeModel, HourAngleRange }
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbElevationRange {
  import ArbEnumerated._

  // needed to prevent diverging implicits.
  implicit val arbIntDeciValue: Arbitrary[AirmassRange.IntDeciValue] = intervalClosedArbitrary
  implicit val arbIntDeciHour: Arbitrary[HourAngleRange.IntDeciHour] = intervalClosedArbitrary

  implicit val arbAirmassRange: Arbitrary[AirmassRange] =
    Arbitrary {
      for {
        val1 <- arbitrary[AirmassRange.IntDeciValue]
        val2 <- arbitrary[AirmassRange.IntDeciValue]
      } yield
        if (val1.value < val2.value) AirmassRange(val1, val2).get
        else AirmassRange(val2, val1).get
    }

  implicit val cogAirmassRange: Cogen[AirmassRange] =
    Cogen[(AirmassRange.IntDeciValue, AirmassRange.IntDeciValue)].contramap(amr =>
      (amr.deciMin, amr.deciMax)
    )

  implicit val arbAirmassRangeCreate: Arbitrary[AirmassRange.Create] =
    Arbitrary {
      for {
        min        <- arbitrary[Int]
        max        <- arbitrary[Int]
        arc1        = AirmassRange.Create(min, max)
        // make at least some of them valid
        minInRange <- arbitrary[AirmassRange.IntDeciValue]
        maxInRange <- arbitrary[AirmassRange.IntDeciValue]
        arc2        = AirmassRange.Create(minInRange.value, maxInRange.value)
        arc        <- Gen.oneOf(arc1, arc2)
      } yield arc
    }

  implicit val cogAirmassRangeCreate: Cogen[AirmassRange.Create] =
    Cogen[(Int, Int)].contramap(c => (c.deciMin, c.deciMax))

  implicit val arbHourAngleRange: Arbitrary[HourAngleRange] =
    Arbitrary {
      for {
        val1 <- arbitrary[HourAngleRange.IntDeciHour]
        val2 <- arbitrary[HourAngleRange.IntDeciHour]
      } yield
        if (val1.value < val2.value) HourAngleRange(val1, val2).get
        else HourAngleRange(val2, val1).get
    }

  implicit val cogHourAngleRange: Cogen[HourAngleRange] =
    Cogen[(HourAngleRange.IntDeciHour, HourAngleRange.IntDeciHour)].contramap(har =>
      (har.deciMin, har.deciMax)
    )

  implicit val arbHourAngleRangeCreate: Arbitrary[HourAngleRange.Create] =
    Arbitrary {
      for {
        min        <- arbitrary[Int]
        max        <- arbitrary[Int]
        harc1       = HourAngleRange.Create(min, max)
        // make at least some of them valid
        minInRange <- arbitrary[HourAngleRange.IntDeciHour]
        maxInRange <- arbitrary[HourAngleRange.IntDeciHour]
        harc2       = HourAngleRange.Create(minInRange.value, maxInRange.value)
        harc       <- Gen.oneOf(harc1, harc2)
      } yield harc
    }

  implicit val cogHourAngleRangeCreate: Cogen[HourAngleRange.Create] =
    Cogen[(Int, Int)].contramap(c => (c.deciMin, c.deciMax))

  implicit val arbElevationRange: Arbitrary[ElevationRangeModel] =
    Arbitrary {
      for {
        airmassRange   <- arbitrary[AirmassRange]
        hourAngleRange <- arbitrary[HourAngleRange]
        elevationRange <- Gen.oneOf(airmassRange, hourAngleRange)
      } yield elevationRange
    }

  implicit val cogElevationRange: Cogen[ElevationRangeModel] =
    Cogen[Either[AirmassRange, HourAngleRange]].contramap {
      case airmass: AirmassRange     => Left(airmass)
      case hourAngle: HourAngleRange => Right(hourAngle)
    }

  implicit val arbElevationRangeCreate: Arbitrary[ElevationRangeModel.Create] =
    Arbitrary {
      for {
        airmassRange   <- arbitrary[Option[AirmassRange.Create]]
        hourAngleRange <- arbitrary[Option[HourAngleRange.Create]]
      } yield ElevationRangeModel.Create(airmassRange, hourAngleRange)
    }

  implicit val cogElevationRangeCreate: Cogen[ElevationRangeModel.Create] =
    Cogen[(Option[AirmassRange.Create], Option[HourAngleRange.Create])].contramap { ermc =>
      (ermc.airmassRange, ermc.hourAngleRange)
    }
}

object ArbElevationRange extends ArbElevationRange

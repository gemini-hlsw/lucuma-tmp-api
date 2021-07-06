// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.arb

import eu.timepit.refined.api.Refined
import eu.timepit.refined.scalacheck._
import lucuma.core.util.arb.ArbEnumerated
import lucuma.odb.api.model.{AirmassRange, ElevationRangeModel, HourAngleRange}
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbElevationRange {
  import ArbEnumerated._

  implicit val arbDecimalValue: Arbitrary[AirmassRange.DecimalValue] =
    Arbitrary {
      Gen.chooseNum(AirmassRange.MinValue.doubleValue, AirmassRange.MaxValue.doubleValue).map { d =>
        Refined.unsafeApply[BigDecimal, AirmassRange.Value](d)
      }
    }

  implicit val arbDecimalHour: Arbitrary[HourAngleRange.DecimalHour] =
    Arbitrary {
      Gen.chooseNum(HourAngleRange.MinHour.doubleValue, HourAngleRange.MaxHour.doubleValue).map { d =>
        Refined.unsafeApply[BigDecimal, HourAngleRange.DeciHour](d)
      }
    }


  implicit val arbAirmassRange: Arbitrary[AirmassRange] =
    Arbitrary {
      for {
        val1 <- arbitrary[AirmassRange.DecimalValue]
        val2 <- arbitrary[AirmassRange.DecimalValue]
      } yield
        if (val1.value < val2.value) AirmassRange(val1, val2).get
        else AirmassRange(val2, val1).get
    }

  implicit val cogAirmassRange: Cogen[AirmassRange] =
    Cogen[(AirmassRange.DecimalValue, AirmassRange.DecimalValue)].contramap(amr =>
      (amr.min, amr.max)
    )

  implicit val arbAirmassRangeCreate: Arbitrary[AirmassRange.Create] =
    Arbitrary {
      for {
        min        <- arbitrary[BigDecimal]
        max        <- arbitrary[BigDecimal]
        arc1        = AirmassRange.Create(min, max)
        // make at least some of them valid
        minInRange <- arbitrary[AirmassRange.DecimalValue]
        maxInRange <- arbitrary[AirmassRange.DecimalValue]
        arc2        = AirmassRange.Create(minInRange.value, maxInRange.value)
        arc        <- Gen.oneOf(arc1, arc2)
      } yield arc
    }

  implicit val cogAirmassRangeCreate: Cogen[AirmassRange.Create] =
    Cogen[(BigDecimal, BigDecimal)].contramap(c => (c.min, c.max))

  implicit val arbHourAngleRange: Arbitrary[HourAngleRange] =
    Arbitrary {
      for {
        val1 <- arbitrary[HourAngleRange.DecimalHour]
        val2 <- arbitrary[HourAngleRange.DecimalHour]
      } yield
        if (val1.value < val2.value) HourAngleRange(val1, val2).get
        else HourAngleRange(val2, val1).get
    }

  implicit val cogHourAngleRange: Cogen[HourAngleRange] =
    Cogen[(HourAngleRange.DecimalHour, HourAngleRange.DecimalHour)].contramap(har =>
      (har.minHours, har.maxHours)
    )

  implicit val arbHourAngleRangeCreate: Arbitrary[HourAngleRange.Create] =
    Arbitrary {
      for {
        min        <- arbitrary[BigDecimal]
        max        <- arbitrary[BigDecimal]
        harc1       = HourAngleRange.Create(min, max)
        // make at least some of them valid
        minInRange <- arbitrary[HourAngleRange.DecimalHour]
        maxInRange <- arbitrary[HourAngleRange.DecimalHour]
        harc2       = HourAngleRange.Create(minInRange.value, maxInRange.value)
        harc       <- Gen.oneOf(harc1, harc2)
      } yield harc
    }

  implicit val cogHourAngleRangeCreate: Cogen[HourAngleRange.Create] =
    Cogen[(BigDecimal, BigDecimal)].contramap(c => (c.minHours, c.maxHours))

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

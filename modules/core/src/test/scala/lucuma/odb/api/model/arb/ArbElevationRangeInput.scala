// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.arb

import clue.data.Input
import eu.timepit.refined.api.Refined
import eu.timepit.refined.scalacheck._
import lucuma.core.model.ElevationRange
import lucuma.core.util.arb.ArbEnumerated
import lucuma.odb.api.model.{AirMassRangeInput, ElevationRangeInput, HourAngleRangeInput}
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbElevationRangeInput {
  import ElevationRange.{AirMass, HourAngle}

  import ArbEnumerated._
  import ArbInput._

  implicit val arbDecimalValue: Arbitrary[AirMass.DecimalValue] =
    Arbitrary {
      Gen.chooseNum(AirMass.MinValue.doubleValue, AirMass.MaxValue.doubleValue).map { d =>
        Refined.unsafeApply[BigDecimal, AirMass.Value](d)
      }
    }

  implicit val arbDecimalHour: Arbitrary[HourAngle.DecimalHour] =
    Arbitrary {
      Gen.chooseNum(HourAngle.MinHour.doubleValue, HourAngle.MaxHour.doubleValue).map { d =>
        Refined.unsafeApply[BigDecimal, HourAngle.Hour](d)
      }
    }


  implicit val arbAirMassRangeInput: Arbitrary[AirMassRangeInput] =
    Arbitrary {
      for {
        min        <- arbitrary[Option[BigDecimal]]
        max        <- arbitrary[Option[BigDecimal]]
        arc1        = AirMassRangeInput(min, max)
        // make at least some of them valid
        minInRange <- arbitrary[Option[AirMass.DecimalValue]]
        maxInRange <- arbitrary[Option[AirMass.DecimalValue]]
        arc2        = AirMassRangeInput(minInRange.map(_.value), maxInRange.map(_.value))
        arc        <- Gen.oneOf(arc1, arc2)
      } yield arc
    }

  implicit val cogAirmassRangeInput: Cogen[AirMassRangeInput] =
    Cogen[(Option[BigDecimal], Option[BigDecimal])].contramap(c => (c.min, c.max))

  implicit val arbHourAngleRangeInput: Arbitrary[HourAngleRangeInput] =
    Arbitrary {
      for {
        min        <- arbitrary[Option[BigDecimal]]
        max        <- arbitrary[Option[BigDecimal]]
        harc1       = HourAngleRangeInput(min, max)
        // make at least some of them valid
        minInRange <- arbitrary[Option[HourAngle.DecimalHour]]
        maxInRange <- arbitrary[Option[HourAngle.DecimalHour]]
        harc2       = HourAngleRangeInput(minInRange.map(_.value), maxInRange.map(_.value))
        harc       <- Gen.oneOf(harc1, harc2)
      } yield harc
    }

  implicit val cogHourAngleRangeInput: Cogen[HourAngleRangeInput] =
    Cogen[(Option[BigDecimal], Option[BigDecimal])].contramap(c => (c.minHours, c.maxHours))

  implicit val arbElevationRangeCreate: Arbitrary[ElevationRangeInput] =
    Arbitrary {
      for {
        airMass   <- arbitrary[Input[AirMassRangeInput]]
        hourAngle <- arbitrary[Input[HourAngleRangeInput]]
      } yield ElevationRangeInput(airMass, hourAngle)
    }

  implicit val cogElevationRangeCreate: Cogen[ElevationRangeInput] =
    Cogen[(Input[AirMassRangeInput], Input[HourAngleRangeInput])].contramap { ermc =>
      (ermc.airMass, ermc.hourAngle)
    }
}

object ArbElevationRangeInput extends ArbElevationRangeInput

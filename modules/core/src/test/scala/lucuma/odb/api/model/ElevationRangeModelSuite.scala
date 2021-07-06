// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.kernel.laws.discipline._
import eu.timepit.refined.cats._
import lucuma.odb.api.model.arb._
import monocle.law.discipline._
import munit._
import org.scalacheck.Prop._

final class ElevationRangeModelSuite extends DisciplineSuite {
  import ArbElevationRange._

  // Laws
  checkAll("Eq[AirmassRange]", EqTests[AirmassRange].eqv)
  checkAll("Eq[AirmassRange.Create]", EqTests[AirmassRange.Create].eqv)
  checkAll("AirmassRange.fromOrderedDecimalValues", PrismTests(AirmassRange.fromOrderedDecimalValues))
  checkAll("Eq[HourAngleRange]", EqTests[HourAngleRange].eqv)
  checkAll("Eq[HourAngleRange.Create]", EqTests[HourAngleRange.Create].eqv)
  checkAll("HourAngleRange.fromDecimalHours", PrismTests(HourAngleRange.fromOrderedDecimalHours))
  checkAll("Eq[ElevationRangeModel]", EqTests[ElevationRangeModel].eqv)
  checkAll("ElevationRangeModel.airmassRange", PrismTests(ElevationRangeModel.airmassRange))
  checkAll("ElevationRangeModel.hourAngleRange", PrismTests(ElevationRangeModel.hourAngleRange))
  checkAll("Eq[ElevationRangeModel.Create]", EqTests[ElevationRangeModel.Create].eqv)

  test("AirmassRange.min") {
    forAll { range: AirmassRange =>
      assertEquals(AirmassRange.min.get(range), range.min)
    }
  }

  test("AirmassRange.max") {
    forAll { range: AirmassRange =>
      assertEquals(AirmassRange.max.get(range), range.max)
    }
  }

  test("HourAngleRange.minHours") {
    forAll { range: HourAngleRange =>
      assertEquals(HourAngleRange.minHours.get(range), range.minHours)
    }
  }

  test("HourAngleRange.maxHours") {
    forAll { range: HourAngleRange =>
      assertEquals(HourAngleRange.maxHours.get(range), range.maxHours)
    }
  }
}

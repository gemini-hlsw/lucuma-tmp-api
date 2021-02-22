// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
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
  checkAll("AirmassRange.fromOrderedDeciVals", PrismTests(AirmassRange.fromOrderedDeciVals))
  checkAll("Eq[HourAngleRange]", EqTests[HourAngleRange].eqv)
  checkAll("Eq[HourAngleRange.Create]", EqTests[HourAngleRange.Create].eqv)
  checkAll("HourAngleRange.fromDeciHours", PrismTests(HourAngleRange.fromOrderedDeciHours))
  checkAll("Eq[ElevationRangeModel]", EqTests[ElevationRangeModel].eqv)
  checkAll("ElevationRangeModel.airmassRange", PrismTests(ElevationRangeModel.airmassRange))
  checkAll("ElevationRangeModel.hourAngleRange", PrismTests(ElevationRangeModel.hourAngleRange))
  checkAll("Eq[ElevationRangeModel.Create]", EqTests[ElevationRangeModel.Create].eqv)

  test("AirmassRange.deciMin") {
    forAll { range: AirmassRange =>
      assertEquals(AirmassRange.deciMin.get(range), range.deciMin)
    }
  }

  test("AirmassRange.deciMax") {
    forAll { range: AirmassRange =>
      assertEquals(AirmassRange.deciMax.get(range), range.deciMax)
    }
  }

  test("HourAngleRange.deciMin") {
    forAll { range: HourAngleRange =>
      assertEquals(HourAngleRange.deciMin.get(range), range.deciMin)
    }
  }

  test("HourAngleRange.deciMax") {
    forAll { range: HourAngleRange =>
      assertEquals(HourAngleRange.deciMax.get(range), range.deciMax)
    }
  }
}

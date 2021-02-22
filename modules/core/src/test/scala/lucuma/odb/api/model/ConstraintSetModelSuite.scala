// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.kernel.laws.discipline._
import cats.syntax.all._
import eu.timepit.refined.cats._
import eu.timepit.refined.scalacheck.string._
import lucuma.core.arb._
import lucuma.core.util.arb._
import lucuma.odb.api.model.arb._
import monocle.law.discipline._
import munit._
import org.scalacheck.Prop._

final class ConstraintSetModelSuite extends DisciplineSuite {
  import ArbConstraintSetModel._
  import ArbElevationRange._
  import ArbEnumerated._
  import ArbGid._

  checkAll("Eq[ConstraintSet]", EqTests[ConstraintSetModel].eqv)
  checkAll("ConstraintSet.id", LensTests(ConstraintSetModel.id))
  checkAll("ConstraintSet.existence", LensTests(ConstraintSetModel.existence))
  checkAll("ConstraintSet.name", LensTests(ConstraintSetModel.name))
  checkAll("ConstraintSet.imageQuality", LensTests(ConstraintSetModel.imageQuality))
  checkAll("ConstraintSet.cloudExtinction", LensTests(ConstraintSetModel.cloudExtinction))
  checkAll("ConstraintSet.skyBackground", LensTests(ConstraintSetModel.skyBackground))
  checkAll("ConstraintSet.waterVapor", LensTests(ConstraintSetModel.waterVapor))
  checkAll("ConstraintSet.elevationRange", LensTests(ConstraintSetModel.elevationRange))
  checkAll("ConstraintSet.airmass", OptionalTests(ConstraintSetModel.airmass))
  checkAll("ConstraintSet.hourAngle", OptionalTests(ConstraintSetModel.hourAngle))
  checkAll("Eq[ConstraintSet.Create]", EqTests[ConstraintSetModel.Create].eqv)
  checkAll("Eq[ConstraintSet.Edit]", EqTests[ConstraintSetModel.Edit].eqv)

  test("ConstraintSet.AirmassDeciMin") {
    forAll { constraints: ConstraintSetModel =>
      val headOption = ConstraintSetModel.airMassDeciMin.headOption(constraints)
      val expected   = constraints.elevationRange match {
        case amr: AirmassRange => amr.deciMin.some
        case _: HourAngleRange => none
      }
      assertEquals(headOption, expected)
    }
  }

  test("ConstraintSet.AirmassDeciMax") {
    forAll { constraints: ConstraintSetModel =>
      val headOption = ConstraintSetModel.airMassDeciMax.headOption(constraints)
      val expected   = constraints.elevationRange match {
        case amr: AirmassRange => amr.deciMax.some
        case _: HourAngleRange => none
      }
      assertEquals(headOption, expected)
    }
  }

  test("ConstraintSet.HourAngleDeciMin") {
    forAll { constraints: ConstraintSetModel =>
      val headOption = ConstraintSetModel.hourAngleDeciMin.headOption(constraints)
      val expected   = constraints.elevationRange match {
        case har: HourAngleRange => har.deciMin.some
        case _: AirmassRange     => none
      }
      assertEquals(headOption, expected)
    }
  }

  test("ConstraintSet.HourAngleDeciMax") {
    forAll { constraints: ConstraintSetModel =>
      val headOption = ConstraintSetModel.hourAngleDeciMax.headOption(constraints)
      val expected   = constraints.elevationRange match {
        case har: HourAngleRange => har.deciMax.some
        case _: AirmassRange     => none
      }
      assertEquals(headOption, expected)
    }
  }
}

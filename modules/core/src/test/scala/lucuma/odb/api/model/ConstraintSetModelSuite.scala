// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.kernel.laws.discipline._
import cats.syntax.all._
import lucuma.core.util.arb._
import lucuma.odb.api.model.arb._
import monocle.law.discipline._
import munit._
import org.scalacheck.Prop._

final class ConstraintSetModelSuite extends DisciplineSuite {
  import ArbConstraintSetModel._
  import ArbElevationRange._
  import ArbEnumerated._

  checkAll("Eq[ConstraintSet]", EqTests[ConstraintSetModel].eqv)
  checkAll("ConstraintSet.imageQuality", LensTests(ConstraintSetModel.imageQuality))
  checkAll("ConstraintSet.cloudExtinction", LensTests(ConstraintSetModel.cloudExtinction))
  checkAll("ConstraintSet.skyBackground", LensTests(ConstraintSetModel.skyBackground))
  checkAll("ConstraintSet.waterVapor", LensTests(ConstraintSetModel.waterVapor))
  checkAll("ConstraintSet.elevationRange", LensTests(ConstraintSetModel.elevationRange))
  checkAll("ConstraintSet.airmass", OptionalTests(ConstraintSetModel.airmass))
  checkAll("ConstraintSet.hourAngle", OptionalTests(ConstraintSetModel.hourAngle))
  checkAll("Eq[ConstraintSet.Create]", EqTests[ConstraintSetModel.Create].eqv)
  checkAll("Eq[ConstraintSet.Edit]", EqTests[ConstraintSetModel.Edit].eqv)

  test("ConstraintSet.AirmassMin") {
    forAll { constraints: ConstraintSetModel =>
      val headOption = ConstraintSetModel.airMassMin.headOption(constraints)
      val expected   = constraints.elevationRange match {
        case amr: AirmassRange => amr.min.some
        case _: HourAngleRange => none
      }
      assertEquals(headOption, expected)
    }
  }

  test("ConstraintSet.AirmassMax") {
    forAll { constraints: ConstraintSetModel =>
      val headOption = ConstraintSetModel.airMassMax.headOption(constraints)
      val expected   = constraints.elevationRange match {
        case amr: AirmassRange => amr.max.some
        case _: HourAngleRange => none
      }
      assertEquals(headOption, expected)
    }
  }

  test("ConstraintSet.HourAngleMin") {
    forAll { constraints: ConstraintSetModel =>
      val headOption = ConstraintSetModel.hourAngleMin.headOption(constraints)
      val expected   = constraints.elevationRange match {
        case har: HourAngleRange => har.minHours.some
        case _: AirmassRange     => none
      }
      assertEquals(headOption, expected)
    }
  }

  test("ConstraintSet.HourAngleMax") {
    forAll { constraints: ConstraintSetModel =>
      val headOption = ConstraintSetModel.hourAngleMax.headOption(constraints)
      val expected   = constraints.elevationRange match {
        case har: HourAngleRange => har.maxHours.some
        case _: AirmassRange     => none
      }
      assertEquals(headOption, expected)
    }
  }
}

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.arb._

import cats.kernel.laws.discipline.EqTests
import munit.DisciplineSuite
//import monocle.law.discipline._

final class StepModelSuite extends DisciplineSuite {

  import ArbStepModel._

  checkAll("StepModel.StepModel", EqTests[StepModel[Int]].eqv)
  checkAll("StepModel.CreateStep", EqTests[StepModel.CreateStep[Int]].eqv)

}

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.arb._

import cats.kernel.laws.discipline.EqTests
import monocle.law.discipline.OptionalTests
import munit.DisciplineSuite

final class StepModelSuite extends DisciplineSuite {

  import ArbGcalModel._
  import ArbStepModel._
  import ArbOffsetModel._

  checkAll("StepModel.StepModel",  EqTests[StepModel[Int]].eqv)
  checkAll("StepModel.CreateStep", EqTests[StepModel.CreateStep[Int]].eqv)

  checkAll("StepModel.CreateStep.instrumentConfig", OptionalTests(StepModel.CreateStep.instrumentConfig[Int]))
  checkAll("StepModel.CreateStep.gcalConfig",    OptionalTests(StepModel.CreateStep.gcalConfig[Int]))
  checkAll("StepModel.CreateStep.offset",        OptionalTests(StepModel.CreateStep.offset[Int]))
}

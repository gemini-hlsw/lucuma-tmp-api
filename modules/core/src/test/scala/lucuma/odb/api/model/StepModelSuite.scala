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

  checkAll("StepModel.StepModel",  EqTests[StepConfig[Int]].eqv)
  checkAll("StepModel.CreateStep", EqTests[StepConfig.CreateStepConfig[Int]].eqv)

  checkAll("StepModel.CreateStep.instrumentConfig", OptionalTests(StepConfig.CreateStepConfig.instrumentConfig[Int]))
  checkAll("StepModel.CreateStep.gcalConfig",    OptionalTests(StepConfig.CreateStepConfig.gcalConfig[Int]))
  checkAll("StepModel.CreateStep.offset",        OptionalTests(StepConfig.CreateStepConfig.offset[Int]))
}

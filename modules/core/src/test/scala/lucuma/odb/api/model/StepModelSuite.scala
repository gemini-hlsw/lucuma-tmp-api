// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
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

  checkAll("StepModel",                  EqTests[StepModel[Int]].eqv)
  checkAll("StepModel",                  EqTests[StepModel.Create[Int]].eqv)

  checkAll("StepConfig",                  EqTests[StepConfig[Int]].eqv)
  checkAll("StepConfig.CreateStepConfig", EqTests[StepConfig.CreateStepConfig[Int]].eqv)

  checkAll("StepConfig.CreateStep.instrumentConfig", OptionalTests(StepConfig.CreateStepConfig.instrumentConfig[Int]))
  checkAll("StepConfig.CreateStep.gcalConfig",       OptionalTests(StepConfig.CreateStepConfig.gcalConfig[Int]))
  checkAll("StepConfig.CreateStep.offset",           OptionalTests(StepConfig.CreateStepConfig.offset[Int]))
}

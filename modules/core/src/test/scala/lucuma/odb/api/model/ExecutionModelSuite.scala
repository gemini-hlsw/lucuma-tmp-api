// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.arb._
import cats.kernel.laws.discipline.EqTests
import munit.DisciplineSuite


final class ExecutionModelSuite extends DisciplineSuite {

  import ArbExecutionModel._

  checkAll("ExecutionModelModel",            EqTests[ExecutionModel].eqv)
  checkAll("ExecutionModelModel.Create",     EqTests[ExecutionModel.Create].eqv)

  checkAll("ExecutionModel.Create",          EqTests[ExecutionModel.Create].eqv)
  checkAll("ExecutionModel.CreateGmosNorth", EqTests[ExecutionModel.CreateGmosNorth].eqv)
  checkAll("ExecutionModel.CreateGmosSouth", EqTests[ExecutionModel.CreateGmosSouth].eqv)

}

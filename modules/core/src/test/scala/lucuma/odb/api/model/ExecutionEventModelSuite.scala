// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.arb._

import cats.kernel.laws.discipline.EqTests
import munit.DisciplineSuite

final class ExecutionEventModelSuite extends DisciplineSuite {

  import ArbExecutionEventModel._

  checkAll("SequenceEvent", EqTests[ExecutionEventModel.SequenceEvent].eqv)
  checkAll("StepEvent",     EqTests[ExecutionEventModel.StepEvent].eqv)
  checkAll("DatasetEvent",  EqTests[ExecutionEventModel.DatasetEvent].eqv)
  checkAll("ExecutionEvent", EqTests[ExecutionEventModel].eqv)

}

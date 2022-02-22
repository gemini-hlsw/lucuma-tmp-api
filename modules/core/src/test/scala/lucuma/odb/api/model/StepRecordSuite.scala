// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.arb._

import cats.kernel.laws.discipline.EqTests
import munit.DisciplineSuite

final class StepRecordSuite extends DisciplineSuite {

  import ArbGmosModel._
  import ArbStepRecord._

  checkAll("StepRecord",       EqTests[StepRecord[GmosModel.NorthDynamic]].eqv)
  checkAll("StepRecord.Input", EqTests[StepRecord.Input[GmosModel.NorthDynamic]].eqv)

}

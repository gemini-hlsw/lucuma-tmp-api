// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.arb._

import cats.kernel.laws.discipline.EqTests
import munit.DisciplineSuite

final class AsterismSuite extends DisciplineSuite {

  import ArbAsterismModel._

  checkAll("AsterismModel.Create", EqTests[AsterismModel.Create].eqv)
  checkAll("AsterismModel.Edit", EqTests[AsterismModel.Edit].eqv)
}

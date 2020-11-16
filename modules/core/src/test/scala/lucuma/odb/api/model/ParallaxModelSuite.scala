// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.kernel.laws.discipline.EqTests
import lucuma.odb.api.model.arb._
import munit.DisciplineSuite

final class ParallaxModelSuite extends DisciplineSuite {

  import ArbParallaxModel._

  checkAll("ParallaxModel.Input", EqTests[ParallaxModel.Input].eqv)

}

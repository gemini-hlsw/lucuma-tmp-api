// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.arb._

import cats.kernel.laws.discipline.EqTests
import munit.DisciplineSuite

final class TargetModelSuite extends DisciplineSuite {

  import ArbCatalogIdModel._
  import ArbCoordinatesModel._
  import ArbDeclinationModel._
  import ArbParallaxModel._
  import ArbRightAscensionModel._

  checkAll("CatalogIdModel.Input", EqTests[CatalogIdModel.Input].eqv)
  checkAll("CoordinatesModel.Input", EqTests[CoordinatesModel.Input].eqv)
  checkAll("DeclinationModel.Input", EqTests[DeclinationModel.Input].eqv)
  checkAll("ParallaxModel.Input", EqTests[ParallaxModel.Input].eqv)
  checkAll("RightAscensionModel.Input", EqTests[RightAscensionModel.Input].eqv)

}

// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.arb._

import cats.kernel.laws.discipline.EqTests
import munit.DisciplineSuite

final class TargetModelSuite extends DisciplineSuite {

  import ArbCatalogIdModel._
  import ArbCoordinatesModel._
  import ArbDeclinationModel._
  import ArbMagnitudeModel._
  import ArbParallaxModel._
  import ArbProperMotionModel._
  import ArbRadialVelocityModel._
  import ArbRightAscensionModel._
  import ArbTargetModel._

  checkAll("CatalogIdModel.Input", EqTests[CatalogIdModel.Input].eqv)
  checkAll("CoordinatesModel.Input", EqTests[CoordinatesModel.Input].eqv)
  checkAll("DeclinationModel.Input", EqTests[DeclinationModel.Input].eqv)
  checkAll("MagnitudeModel.Create", EqTests[MagnitudeModel.Create].eqv)
  checkAll("MagnitudeModel.Edit", EqTests[MagnitudeModel.Edit].eqv)
  checkAll("MagnitudeModel.EditAction", EqTests[MagnitudeModel.EditAction].eqv)
  checkAll("MagnitudeModel.EditList", EqTests[MagnitudeModel.EditList].eqv)
  checkAll("ParallaxModel.Input", EqTests[ParallaxModel.Input].eqv)
  checkAll("ProperMotionModel.Input", EqTests[ProperMotionModel.Input].eqv)
  checkAll("RadialVelocityModel.Input", EqTests[RadialVelocityModel.Input].eqv)
  checkAll("RightAscensionModel.Input", EqTests[RightAscensionModel.Input].eqv)
  checkAll("TargetModel.Create", EqTests[TargetModel.Create].eqv)
  checkAll("TargetModel.CreateNonsidereal", EqTests[TargetModel.CreateNonsidereal].eqv)
  checkAll("TargetModel.CreateSidereal", EqTests[TargetModel.CreateSidereal].eqv)
  checkAll("TargetModel.Edit", EqTests[TargetModel.Edit].eqv)
  checkAll("TargetModel.EditNonsidereal", EqTests[TargetModel.EditNonsidereal].eqv)
  checkAll("TargetModel.EditSidereal", EqTests[TargetModel.EditSidereal].eqv)
  checkAll("TargetModel.EditTargetAction", EqTests[TargetModel.EditTargetAction].eqv)
  checkAll("TargetModel.EditTargetList", EqTests[TargetModel.EditTargetList].eqv)

}

// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.arb._
import cats.kernel.laws.discipline.EqTests
import lucuma.odb.api.model.targetModel._
import munit.DisciplineSuite

final class TargetModelSuite extends DisciplineSuite {

  import ArbCatalogInfoModel._
  import ArbCoordinatesModel._
  import ArbDeclinationModel._
  import ArbParallaxModel._
  import ArbProperMotionModel._
  import ArbRadialVelocityModel._
  import ArbRightAscensionModel._
  import ArbTargetModel._

  checkAll("CatalogIdModel.Input", EqTests[CatalogInfoModel.Input].eqv)
  checkAll("CoordinatesModel.Input", EqTests[CoordinatesModel.Input].eqv)
  checkAll("DeclinationModel.Input", EqTests[DeclinationModel.Input].eqv)
  checkAll("ParallaxModel.Input", EqTests[ParallaxModel.Input].eqv)
  checkAll("ProperMotionModel.Input", EqTests[ProperMotionModel.Input].eqv)
  checkAll("RadialVelocityModel.Input", EqTests[RadialVelocityModel.Input].eqv)
  checkAll("RightAscensionModel.Input", EqTests[RightAscensionModel.Input].eqv)

  checkAll("TargetModel", EqTests[TargetModel].eqv)
  checkAll("TargetEnvironmentModel", EqTests[TargetEnvironmentModel].eqv)

  checkAll("TargetModel.Create", EqTests[TargetModel.Create].eqv)
  checkAll("CreateNonsiderealInput", EqTests[CreateNonsiderealInput].eqv)
  checkAll("CreateSiderealInput", EqTests[CreateSiderealInput].eqv)

  checkAll("EditSiderealInput", EqTests[EditSiderealInput].eqv)
  checkAll("EditNonsiderealInput", EqTests[EditNonsiderealInput].eqv)
  checkAll("EditTargetInput", EqTests[EditAsterismInput].eqv)

  checkAll("TargetEnvironmentModel.Create", EqTests[TargetEnvironmentModel.Create].eqv)

}

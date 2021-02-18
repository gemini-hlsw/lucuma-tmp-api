// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.arb._

import cats.kernel.laws.discipline.EqTests
import monocle.law.discipline._
import munit.DisciplineSuite

final class ConfigModelSuite extends DisciplineSuite {

  import ArbConfigModel._
  import ArbSequenceModel._
  import ArbGmosModel._

  checkAll("ConfigModel",           EqTests[ConfigModel].eqv)
  checkAll("ConfigModel.Create",    EqTests[ConfigModel.Create].eqv)
  checkAll("ConfigModel.GmosNorth", EqTests[ConfigModel.GmosNorth].eqv)
  checkAll("ConfigModel.GmosSouth", EqTests[ConfigModel.GmosSouth].eqv)

  checkAll("ConfigModel.GmosNorth.manual", LensTests(ConfigModel.GmosNorth.manual))
  checkAll("ConfigModel.GmosSouth.manual", LensTests(ConfigModel.GmosSouth.manual))
}

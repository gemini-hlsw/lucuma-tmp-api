// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.arb._

import cats.kernel.laws.discipline.EqTests
import munit.DisciplineSuite


final class InstrumentConfigModelSuite extends DisciplineSuite {

  import ArbInstrumentConfigModel._

  checkAll("InstrumentConfigModel",            EqTests[InstrumentConfigModel].eqv)
  checkAll("InstrumentConfigModel.Create",     EqTests[InstrumentConfigModel.Create].eqv)

  checkAll("InstrumentConfig",                 EqTests[InstrumentConfigModel].eqv)
  checkAll("InstrumentConfig.Create",          EqTests[InstrumentConfigModel.Create].eqv)
  checkAll("InstrumentConfig.CreateGmosNorth", EqTests[InstrumentConfigModel.CreateGmosNorth].eqv)
  checkAll("InstrumentConfig.CreateGmosSouth", EqTests[InstrumentConfigModel.CreateGmosSouth].eqv)

}

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.SequenceModel._
import lucuma.odb.api.model.arb._

import cats.kernel.laws.discipline.EqTests
import monocle.law.discipline._
import munit.DisciplineSuite

final class SequenceModelSuite extends DisciplineSuite {

  import ArbSequenceModel._

  checkAll("BreakpointStep",       EqTests[BreakpointStep[Int]].eqv)
  checkAll("CreateBreakpointStep", EqTests[BreakpointStep.Create[Int]].eqv)

  checkAll("Atom",                 EqTests[Atom[Int]].eqv)
  checkAll("CreateAtom",           EqTests[Atom.Create[Int]].eqv)

  checkAll("Sequence",             EqTests[Sequence[String]].eqv)
  checkAll("Sequence.Create",      EqTests[Sequence[String]].eqv)

  checkAll("Config",               EqTests[Config[Int, String]].eqv)
  checkAll("Config.Create",        EqTests[Config.Create[Int, String]].eqv)

  checkAll("InstrumentConfig",                 EqTests[InstrumentConfig].eqv)
  checkAll("InstrumentConfig.Create",          EqTests[InstrumentConfig.Create].eqv)
  checkAll("InstrumentConfig.CreateGmosNorth", EqTests[InstrumentConfig.CreateGmosNorth].eqv)
  checkAll("InstrumentConfig.CreateGmosSouth", EqTests[InstrumentConfig.CreateGmosSouth].eqv)

  checkAll("Config.static",      LensTests(Config.static[Int, String]))
  checkAll("Config.acquisition", LensTests(Config.acquisition[Int, String]))
  checkAll("Config.science",     LensTests(Config.science[Int, String]))

}

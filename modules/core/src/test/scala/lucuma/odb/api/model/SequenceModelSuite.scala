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

  checkAll("Sequence",             EqTests[Sequence[Int, String]].eqv)
  checkAll("Sequence.Create",      EqTests[Sequence.Create[Int, String]].eqv)

  checkAll("GmosModel.North.static",      LensTests(Sequence.static[Int, String]))
  checkAll("GmosModel.North.acquisition", LensTests(Sequence.acquisition[Int, String]))
  checkAll("GmosModel.North.science",     LensTests(Sequence.science[Int, String]))

}

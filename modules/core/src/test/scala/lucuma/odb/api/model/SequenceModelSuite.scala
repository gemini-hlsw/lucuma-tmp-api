// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.SequenceModel._
import lucuma.odb.api.model.arb._

import cats.kernel.laws.discipline.EqTests
import munit.DisciplineSuite

final class SequenceModelSuite extends DisciplineSuite {

  import ArbSequenceModel._

  checkAll("BreakpointStep",       EqTests[BreakpointStep[Int]].eqv)
  checkAll("CreateBreakpointStep", EqTests[CreateBreakpointStep[Int]].eqv)

  checkAll("Atom",       EqTests[Atom[Int]].eqv)
  checkAll("CreateAtom", EqTests[CreateAtom[Int]].eqv)

}

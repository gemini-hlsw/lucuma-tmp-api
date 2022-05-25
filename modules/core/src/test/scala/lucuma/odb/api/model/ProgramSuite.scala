// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.arb._

import cats.kernel.laws.discipline.EqTests
import eu.timepit.refined.cats._
import eu.timepit.refined.scalacheck.all._
import lucuma.core.model.arb.ArbProposal
import lucuma.core.util.arb.{ArbEnumerated, ArbGid}
import monocle.law.discipline.LensTests
import munit.DisciplineSuite

final class ProgramSuite extends DisciplineSuite {
  import ArbEnumerated._
  import ArbGid._
  import ArbProgramModel._
  import ArbProposal._

  checkAll("ProgramModel", EqTests[ProgramModel].eqv)

  checkAll("ProgramModel.id",        LensTests(ProgramModel.id))
  checkAll("ProgramModel.existence", LensTests(ProgramModel.existence))
  checkAll("ProgramModel.name",      LensTests(ProgramModel.name))
  checkAll("ProgramModel.proposal",  LensTests(ProgramModel.proposal))

}

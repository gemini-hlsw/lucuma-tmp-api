// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.arb._

import cats.kernel.laws.discipline.EqTests
import munit.DisciplineSuite

class ProposalClassInputSuite extends DisciplineSuite {
  import ArbProposalClassInput._
  import ProposalClassInput._

  checkAll("Eq[ProposalClassInput]",      EqTests[ProposalClassInput].eqv)
  checkAll("Eq[ClassicalInput]",          EqTests[ClassicalInput].eqv)
  checkAll("Eq[DemoScienceInput]",        EqTests[DemoScienceInput].eqv)
  checkAll("Eq[DirectorsTimeInput]",      EqTests[DirectorsTimeInput].eqv)
  checkAll("Eq[ExchangeInput]",           EqTests[ExchangeInput].eqv)
  checkAll("Eq[FastTurnaroundInput]",     EqTests[FastTurnaroundInput].eqv)
  checkAll("Eq[PoorWeatherInput]",        EqTests[PoorWeatherInput].eqv)
  checkAll("Eq[QueueInput]",              EqTests[QueueInput].eqv)
  checkAll("Eq[SystemVerificationInput]", EqTests[SystemVerificationInput].eqv)
  checkAll("Eq[LargeProgramInput]",       EqTests[LargeProgramInput].eqv)
  checkAll("Eq[IntensiveInput]",          EqTests[IntensiveInput].eqv)
}

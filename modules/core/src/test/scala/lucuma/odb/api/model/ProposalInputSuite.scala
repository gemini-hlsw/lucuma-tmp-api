// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.arb._

import cats.kernel.laws.discipline.EqTests
import eu.timepit.refined.cats._
import eu.timepit.refined.scalacheck.all._
import lucuma.core.util.arb.ArbEnumerated
import monocle.law.discipline.LensTests
import munit.DisciplineSuite

class ProposalInputSuite extends DisciplineSuite {
  import ArbEnumerated._
  import ArbProposalInput._
  
  checkAll("Eq[ProposalInput]", EqTests[ProposalInput].eqv)

  checkAll("Eq[PartnerSplit]",     EqTests[PartnerSplit].eqv)
  checkAll("PartnerSplit.partner", LensTests(PartnerSplit.partner))
  checkAll("PartnerSplit.percent", LensTests(PartnerSplit.percent))

  checkAll("Eq[PartnerSplitInput]", EqTests[ProposalInput.PartnerSplitInput].eqv)
}

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.arb._

import monocle.law.discipline.IsoTests
import monocle.law.discipline.PrismTests
import munit.DisciplineSuite

final class UidSuite extends DisciplineSuite {

  import ArbUid._

  checkAll("isoUuid",    IsoTests(Uid[Step.Id].isoUuid))
  checkAll("fromString", PrismTests(Uid[Step.Id].fromString))
}

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.arb._

import cats.kernel.laws.discipline.EqTests
import munit.DisciplineSuite

final class VisitRecordSuite extends DisciplineSuite {

  import ArbGmosModel._
  import ArbVisitRecord._

  checkAll("VisitRecord", EqTests[VisitRecord.Input[GmosModel.CreateNorthDynamic]].eqv)

}

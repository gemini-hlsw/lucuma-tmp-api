// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.kernel.laws.discipline.EqTests
import cats.kernel.laws.discipline.OrderTests
import lucuma.odb.api.model.arb._
import munit.DisciplineSuite

final class DatasetModelSuite extends DisciplineSuite {

  import ArbDatasetModel._

  checkAll("DatasetModel",     EqTests[DatasetModel].eqv)
  checkAll("DatasetModel.Id",  OrderTests[DatasetModel.Id].order)

}

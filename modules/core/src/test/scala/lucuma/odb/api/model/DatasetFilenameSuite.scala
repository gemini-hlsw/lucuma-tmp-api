// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.arb._

import lucuma.core.optics.laws.discipline.FormatTests
import cats.kernel.laws.discipline.OrderTests
import munit.DisciplineSuite

final class DatasetFilenameSuite extends DisciplineSuite {

  import ArbDatasetFilename._

  checkAll("DatasetFilename", OrderTests[DatasetFilename].order)

  checkAll("DatasetFilename Format", FormatTests(DatasetFilename.fromString).formatWith(genDatasetFilenameString))

}

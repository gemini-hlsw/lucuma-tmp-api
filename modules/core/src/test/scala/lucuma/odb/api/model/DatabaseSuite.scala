// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.arb._
import lucuma.core.model.Observation
import monocle.law.discipline.OptionalTests
import munit.DisciplineSuite

final class DatabaseSuite extends DisciplineSuite {

  import ArbDatabase._
  import ArbVisitRecords._

  checkAll(
    "Database.visitRecordsAtOptional",
    OptionalTests(
      Database.visitRecordsAtOptional(Observation.Id.fromLong(2).get)
    )
  )

}

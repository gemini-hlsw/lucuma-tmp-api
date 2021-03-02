// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{ObservationModel, ProgramModel, TargetModel}
import lucuma.core.model.{Observation, Program, Target}

import munit.DisciplineSuite

final class TargetRepoSpec extends DisciplineSuite with OdbRepoTest {

  test("shareWithObservations") {

    sharingTest[Target.Id, TargetModel, Observation.Id, ObservationModel] { odb => (
        odb.target,
        odb.observation,
        odb.target.shareWithObservations,
        odb.target.unshareWithObservations,
        odb.observation.selectPageForTarget(_)
    )}

  }

  test("shareWithPrograms") {

    sharingTest[Target.Id, TargetModel, Program.Id, ProgramModel] { odb => (
        odb.target,
        odb.program,
        odb.target.shareWithPrograms,
        odb.target.unshareWithPrograms,
        odb.program.selectPageForTarget(_, includeObservations = false)
    )}

  }


}

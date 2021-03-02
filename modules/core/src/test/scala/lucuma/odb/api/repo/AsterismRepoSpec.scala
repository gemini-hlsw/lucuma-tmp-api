// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{AsterismModel, ObservationModel, ProgramModel, TargetModel}
import lucuma.core.model.{Asterism, Observation, Program, Target}

import munit.DisciplineSuite

final class AsterismRepoSpec extends DisciplineSuite with OdbRepoTest {

  test("shareWithObservations") {

    sharingTest[Asterism.Id, AsterismModel, Observation.Id, ObservationModel] { odb => (
        odb.asterism,
        odb.observation,
        odb.asterism.shareWithObservations,
        odb.asterism.unshareWithObservations,
        odb.observation.selectPageForAsterism(_)
    )}

  }

  test("shareWithPrograms") {

    sharingTest[Asterism.Id, AsterismModel, Program.Id, ProgramModel] { odb => (
        odb.asterism,
        odb.program,
        odb.asterism.shareWithPrograms,
        odb.asterism.unshareWithPrograms,
        odb.program.selectPageForAsterism(_, includeObservations = false)
    )}

  }

  test("shareWithTargets") {

    sharingTest[Asterism.Id, AsterismModel, Target.Id, TargetModel] { odb => (
        odb.asterism,
        odb.target,
        odb.asterism.shareWithTargets,
        odb.asterism.unshareWithTargets,
        odb.target.selectPageForAsterism(_)
    )}

  }

}

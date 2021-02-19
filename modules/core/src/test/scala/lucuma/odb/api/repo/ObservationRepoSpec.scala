// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.core.model.{ConstraintSet, Observation}
import lucuma.odb.api.model.{ConstraintSetModel, ObservationModel}
import munit.ScalaCheckSuite

class ObservationRepoSpec extends ScalaCheckSuite with OdbRepoTest {

  test("shareWithConstraintSets") {
    shareWithOneUniqueTest[Observation.Id, ObservationModel, ConstraintSet.Id, ConstraintSetModel] {
      odb =>
        (
          odb.observation,
          odb.constraintSet,
          odb.observation.shareWithConstraintSet,
          odb.constraintSet.selectForObservation(_, true)
        )
    }
  }

    test("unshareWithConstrainSet") {
      unshareWithOneUniqueTest[Observation.Id, ObservationModel, ConstraintSet.Id, ConstraintSetModel] {
      odb =>
        (
          odb.observation,
          odb.constraintSet,
          odb.observation.unshareWithConstraintSet,
          odb.constraintSet.selectForObservation(_, true)
        )
    }
    }
}

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.core.model.{ConstraintSet, Observation}
import lucuma.odb.api.model.{ConstraintSetModel, ObservationModel}
import munit.ScalaCheckSuite

class ConstraintSetRepoSpec extends ScalaCheckSuite with OdbRepoTest {

  test("shareWithObservations") {
    shareOneWithManyUniqueTest[ConstraintSet.Id,
                               ConstraintSetModel,
                               Observation.Id,
                               ObservationModel
    ] { odb =>
      (
        odb.constraintSet,
        odb.observation,
        odb.constraintSet.shareWithObservations,
        odb.observation.selectPageForConstraintSet(_, includeDeleted = true)
      )
    }
  }

  test("unshareWithObservations") {
    unshareOneWithManyUniqueTest[ConstraintSet.Id,
                                 ConstraintSetModel,
                                 Observation.Id,
                                 ObservationModel
    ] { odb =>
      (
        odb.constraintSet,
        odb.observation,
        odb.constraintSet.unshareWithObservations,
        odb.observation.selectPageForConstraintSet(_, includeDeleted = true)
      )
    }
  }
}

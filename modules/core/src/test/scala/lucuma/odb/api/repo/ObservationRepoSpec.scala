// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.core.model.{ConstraintSet, Observation}
import lucuma.odb.api.model.{ConstraintSetModel, ObservationModel}
import clue.data.Input
import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import org.scalacheck.Prop.forAll
import munit.ScalaCheckSuite

final class ObservationRepoSpec extends ScalaCheckSuite with OdbRepoTest {

  import arb.ArbTables._

  test("shareWithConstraintSets") {
    shareWithOneUniqueTest[Observation.Id, ObservationModel, ConstraintSet.Id, ConstraintSetModel] { odb =>
      (
        odb.observation,
        odb.constraintSet,
        odb.observation.shareWithConstraintSet,
        odb.constraintSet.selectForObservation(_, includeDeleted = true)
      )
    }
  }

  test("unshareWithConstrainSet") {
    unshareWithOneUniqueTest[Observation.Id, ObservationModel, ConstraintSet.Id, ConstraintSetModel] { odb =>
      (
        odb.observation,
        odb.constraintSet,
        odb.observation.unshareWithConstraintSet,
        odb.constraintSet.selectForObservation(_, includeDeleted = true)
      )
    }
  }

  property("simple edit") {

    forAll { (t: Tables) =>
      val obs = runTest(t) { odb =>

        val edit = t.observations.values.headOption.map { o =>
          ObservationModel.Edit(o.id, name = Input("Biff"))
        }

        edit.traverse(odb.observation.edit)
      }

      assert(obs.forall(_.name.contains(NonEmptyString.unsafeFrom("Biff"))))
    }

  }

  property("simple non-edit") {

    forAll { (t: Tables) =>
      val obs = runTest(t) { odb =>

        val edit = t.observations.values.headOption.map { o =>
          ObservationModel.Edit(o.id, name = o.name.fold(Input.ignore[String])(n => Input(n.value)))
        }

        edit.traverse(odb.observation.edit)
      }

      assertEquals(obs, t.observations.values.headOption)
    }

  }

}

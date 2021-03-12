// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.core.model.{Asterism, ConstraintSet, Observation, Target}
import lucuma.odb.api.model.{ConstraintSetModel, InputError, ObservationModel, ProgramModel}

import cats.syntax.all._
import clue.data.Input
import eu.timepit.refined.types.numeric.PosLong
import eu.timepit.refined.types.string.NonEmptyString
import org.scalacheck.Prop
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

  private def runEditTest(
    t: Tables
  )(
    f: ObservationModel => ObservationModel.Edit
  ): (ObservationModel, ObservationModel) =

    runTest(t) { odb =>
      for {
        // Insert a program and observation to insure that at least one exists
        p  <- odb.program.insert(new ProgramModel.Create(None, None))
        _  <- odb.observation.insert(new ObservationModel.Create(None, p.id, None, None, None, None, None))

        // Pick whatever the first observation may be
        tʹ    <- odb.tables.get
        before = tʹ.observations.values.head

        // Do the prescribed edit.
        after <- odb.observation.edit(f(before))
      } yield (before, after)
    }

  property("simple edit") {

    forAll { (t: Tables) =>
      val (_, obs) = runEditTest(t) { o =>
        ObservationModel.Edit(o.id, name = Input("Biff"))
      }
      assert(obs.name.contains(NonEmptyString.unsafeFrom("Biff")))
    }

  }

  property("simple non-edit") {

    forAll { (t: Tables) =>
      val (before, after) = runEditTest(t) { o =>
        ObservationModel.Edit(o.id, name = o.name.fold(Input.ignore[String])(n => Input(n.value)))
      }
      assertEquals(after, before)
    }

  }

  private def asterismTest(f: Tables => Input[Asterism.Id]): Prop =
    forAll { (t: Tables) =>
      val in = f(t)
      val (before, after) = runEditTest(t) { o =>
        ObservationModel.Edit(o.id, asterismId = in)
      }
      assertEquals(after.asterismId, in.fold(before.asterismId, None, _.some))
    }

  property("set asterism") {
    asterismTest(_.asterisms.values.headOption.fold(Input.ignore[Asterism.Id])(a => Input.apply(a.id)))
  }

  property("ignore asterism") {
    asterismTest(_ => Input.ignore)
  }

  property("unassign asterism") {
    asterismTest(_ => Input.unassign)
  }

  private def targetTest(f: Tables => Input[Target.Id]): Prop =
    forAll { (t: Tables) =>
      val in = f(t)
      val (before, after) = runEditTest(t) { o =>
        ObservationModel.Edit(o.id, targetId = in)
      }
      assertEquals(after.targetId, in.fold(before.targetId, None, _.some))
    }

  property("set target") {
    targetTest(_.targets.values.headOption.fold(Input.ignore[Target.Id])(t => Input.apply(t.id)))
  }

  property("ignore target") {
    targetTest(_ => Input.ignore)
  }

  property("unassign target") {
    targetTest(_ => Input.unassign)
  }

  property("reject asterism and target") {
    val one = PosLong.unsafeFrom(1L)

    forAll { (t: Tables) =>
      intercept[InputError.Exception] {
        runEditTest(t) { o =>
          ObservationModel.Edit(
            o.id,
            asterismId = Input(Asterism.Id(one)),
            targetId   = Input(Target.Id(one))
          )
        }
      }
      Prop.passed
    }
  }
}

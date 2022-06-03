// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.syntax.all._
import cats.kernel.instances.order._
import clue.data.Input
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.odb.api.model.{Database, ObservationModel, ProgramModel}
import lucuma.odb.api.model.arb.ArbDatabase
import org.scalacheck.Prop.forAll
import munit.ScalaCheckSuite

final class ObservationRepoSpec extends ScalaCheckSuite with OdbRepoTest {

  import ArbDatabase._

  private def randomSelect(
    database: Database,
    indices:  List[Int]
  ): List[ObservationModel] = {
      val size: Int =
        database.observations.rows.size

      val keep: Set[Int]            =
        if (size === 0) Set.empty[Int] else indices.map(i => (i % size).abs).toSet

      database.observations.rows.zipWithIndex.collect {
        case ((_, o), i) if keep(i) => o
      }.toList.sortBy(_.id)

  }

  property("selectPageForObservations") {
    forAll { (db: Database, indices: List[Int]) =>

      val expected = randomSelect(db, indices).filter(_.existence.isPresent).map(_.id)
      val obtained = runTest(db) { _.observation.selectPageForObservations(expected.toSet) }.nodes.map(_.id)

      assertEquals(obtained, expected)
    }
  }

  property("selectPageForObservations with deleted") {
    forAll { (t: Database, indices: List[Int]) =>

      val expected = randomSelect(t, indices).map(_.id)
      val obtained = runTest(t) { _.observation.selectPageForObservations(expected.toSet, includeDeleted = true) }.nodes.map(_.id)

      assertEquals(obtained, expected)
    }
  }

  property("selectPageForObservations with first") {
    forAll { (t: Database, indices: List[Int], first: Int) =>

      val limitedFirst = if (t.observations.rows.size === 0) 0 else (first % t.observations.rows.size).abs

      val expected = randomSelect(t, indices).filter(_.existence.isPresent).map(_.id)
      val obtained = runTest(t) { _.observation.selectPageForObservations(expected.toSet, count = limitedFirst.some ) }.nodes.map(_.id)

      assertEquals(obtained, expected.take(limitedFirst))
    }
  }

  private def runEditTest(
    t: Database
  )(
    f: ObservationModel => ObservationModel.EditInput
  ): (ObservationModel, ObservationModel) =

    runTest(t) { odb =>
      for {
        // Insert a program and observation to insure that at least one exists
        p  <- odb.program.insert(ProgramModel.CreateInput(ProgramModel.PropertiesInput.Empty.some))
        _  <- odb.observation.insert(ObservationModel.CreateInput.empty(p.newProgram.id))

        // Pick whatever the first observation may be
        tʹ    <- odb.database.get
        before = tʹ.observations.rows.values.head

        // Do the prescribed edit.
        after <- odb.observation.edit(f(before)).map(_.head)
      } yield (before, after)
    }

  property("simple edit") {

    forAll { (t: Database) =>
      val (_, obs) = runEditTest(t) { o =>
        ObservationModel.EditInput(
          ObservationModel.SelectInput.observationId(o.id),
          ObservationModel.PropertiesInput(
            subtitle = Input(NonEmptyString.unsafeFrom("Biff"))
          )
        )
      }
      assert(obs.subtitle.contains(NonEmptyString.unsafeFrom("Biff")))
    }

  }

  property("simple non-edit") {
    forAll { (t: Database) =>
      val (before, after) = runEditTest(t) { o =>
        ObservationModel.EditInput(
          ObservationModel.SelectInput.observationId(o.id),
          ObservationModel.PropertiesInput(
            subtitle = o.subtitle.fold(Input.ignore[NonEmptyString])(n => Input(n))
          )
        )
      }
      assertEquals(after, before)
    }

  }

}

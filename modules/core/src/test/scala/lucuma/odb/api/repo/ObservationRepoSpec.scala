// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.syntax.all._
import cats.kernel.instances.order._
import clue.data.Input
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.odb.api.model.{Database, Existence, ObservationModel, ProgramModel, WhereObservationInput}
import lucuma.odb.api.model.arb.ArbDatabase
import lucuma.odb.api.model.query.{WhereEqInput, WhereOrderInput}
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

  property("selectWhere") {
    forAll { (db: Database, indices: List[Int]) =>

      val expected = randomSelect(db, indices).filter(_.existence.isPresent).map(_.id)
      val obtained = runTest(db) {
        _.observation.selectWhere(
          WhereObservationInput(id = WhereOrderInput.IN(expected).some),
          None,
          None
        )
      }.limitedValues.map(_.id)

      assertEquals(obtained, expected)
    }
  }

  property("selectWhere with deleted") {
    forAll { (t: Database, indices: List[Int]) =>

      val expected = randomSelect(t, indices).map(_.id)
      val obtained = runTest(t) {
        _.observation.selectWhere(
          WhereObservationInput(
            id        = WhereOrderInput.IN(expected).some,
            existence = WhereEqInput.ANY[Existence].some
          ),
          None,
          None
        )
      }.limitedValues.map(_.id)

      assertEquals(obtained, expected)
    }
  }

  property("selectWhere with limit") {
    forAll { (t: Database, indices: List[Int], first: Int) =>

      val limitedFirst = if (t.observations.rows.size === 0) 0 else (first % t.observations.rows.size).abs

      val expected = randomSelect(t, indices).filter(_.existence.isPresent).map(_.id)
      val obtained = runTest(t) {
        _.observation.selectWhere(
          WhereObservationInput(
            id = WhereOrderInput.IN(expected).some
          ),
          None,
          NonNegInt.unsafeFrom(limitedFirst).some
        )
      }.limitedValues.map(_.id)

      assertEquals(obtained, expected.take(limitedFirst))
    }
  }

  private def runEditTest(
    d: Database
  )(
    f: ObservationModel => ObservationModel.UpdateInput
  ): (ObservationModel, ObservationModel) =

    runTest(d) { odb =>
      for {
        // Insert a program and observation to insure that at least one exists
        p  <- odb.program.insert(ProgramModel.CreateInput(ProgramModel.PropertiesInput.Empty.some))
        _  <- odb.observation.insert(ObservationModel.CreateInput.empty(p.program.id))

        // Pick whatever the first observation may be
        dʹ    <- odb.database.get
        before = dʹ.observations.rows.values.head

        // Do the prescribed edit.
        after <- odb.observation.update(f(before)).map(_.allValues.head)
      } yield (before, after)
    }

  property("simple edit") {

    forAll { (t: Database) =>
      val (_, obs) = runEditTest(t) { o =>
        ObservationModel.UpdateInput(
          ObservationModel.PropertiesInput(
            subtitle = Input(NonEmptyString.unsafeFrom("Biff"))
          ),
          WhereObservationInput.MatchAll.withId(o.id).some,
          None
        )
      }
      assert(obs.subtitle.contains(NonEmptyString.unsafeFrom("Biff")))
    }

  }

  property("simple non-edit") {
    forAll { (t: Database) =>
      val (before, after) = runEditTest(t) { o =>
        ObservationModel.UpdateInput(
          ObservationModel.PropertiesInput(
            subtitle = o.subtitle.fold(Input.ignore[NonEmptyString])(n => Input(n))
          ),
          WhereObservationInput.MatchAll.withId(o.id).some,
          None
        )
      }
      assertEquals(after, before)
    }

  }

}

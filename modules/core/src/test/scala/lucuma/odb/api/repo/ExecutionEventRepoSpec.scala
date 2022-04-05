// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.effect.IO
import cats.syntax.all._
import cats.implicits.catsKernelOrderingForOrder
import lucuma.core.model.Observation
import lucuma.odb.api.model.{Database, Step, VisitRecords}
import lucuma.odb.api.model.arb.ArbDatabase
import lucuma.odb.api.model.ExecutionEventModel.{StepEvent, StepStageType}
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

final class ExecutionEventRepoSpec extends ScalaCheckSuite with OdbRepoTest {

  import ArbDatabase._

  val genDatabase: Gen[Database] = arbDatabaseWithSequencesAndEvents.arbitrary

  property("selectExecutedStepsForObservation") {

    forAll(genDatabase) { (db: Database) =>

      val actual = runTest(db) { repo =>
        db.observations.rows.values.toList.traverse { o =>
          db.visitRecords.get(o.id) match {
            case Some(VisitRecords.GmosNorth(_)) =>
              repo
                .executionEvent
                .selectStepsForObservation(o.id, VisitRecords.listGmosNorthVisits)
                .map(_.filter(_.isExecuted).map(sr => (sr.observationId, sr.stepId)))
            case Some(VisitRecords.GmosSouth(_)) =>
              repo
                .executionEvent
                .selectStepsForObservation(o.id, VisitRecords.listGmosSouthVisits)
                .map(_.filter(_.isExecuted).map(sr => (sr.observationId, sr.stepId)))
            case _                                =>
              IO.pure(List.empty[(Observation.Id, Step.Id)])
          }
        }
      }.flatten
       .groupBy(_._1)
       .view
       .mapValues(_.map(_._2).toSet)
       .toMap

      val expected = db.executionEvents.rows.values.toList.collect {
        case StepEvent(_, observationId, _, stepId, _, _, stage) if stage == StepStageType.EndStep =>
          (observationId, stepId)
      }.groupBy(_._1)
       .view
       .mapValues(_.map(_._2).toSet)
       .toMap

      assertEquals(actual, expected)
    }

  }

  property("selectExecutedStepsForObservation is ordered") {

    forAll(genDatabase) { (db: Database) =>

      val actual = runTest(db) { repo =>
        db.observations.rows.keys.toList.traverse { oid =>
          repo
            .executionEvent
            .selectExistentialStepsForObservation(oid)
        }
      }

      val sorted = actual.map(_.sortBy(e => (e.endTime, e.stepId)))

      assertEquals(actual, sorted)
    }

  }
}

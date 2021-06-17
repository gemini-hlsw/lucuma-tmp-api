// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

//import lucuma.odb.api.model.InstrumentConfigModel
import lucuma.odb.api.model.ExecutionEventModel.{StepEvent, StepStageType}

//import cats.data.State
//import cats.effect.IO
import cats.syntax.all._
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

final class ExecutionEventRepoSpec extends ScalaCheckSuite with OdbRepoTest {

  import arb.ArbTables._

  val genTables: Gen[Tables] = arbTablesWithSequencesAndEvents.arbitrary

  property("selectExecutedStepsForObservation") {

    forAll(genTables) { (t: Tables) =>

      val actual = runTest(t) { repo =>
        t.observations.keys.toList.traverse { oid =>
          repo
            .executionEvent
            .selectExecutedStepsForObservation(oid)
            .tupleLeft(oid)
        }
      }.filter(_._2.nonEmpty)
       .groupBy(_._1)
       .view
       .mapValues(_.flatMap(_._2).map(_.stepId).toSet)
       .toMap

      val expected = t.executionEvents.values.toList.collect {
        case StepEvent(_, observationId, _, _, stepId, _, stage) if stage == StepStageType.EndStep =>
          (observationId, stepId)
      }.groupBy(_._1)
       .view
       .mapValues(_.map(_._2).toSet)
       .toMap

      assertEquals(actual, expected)
    }

  }

  /*
  property("selectRemainingAtoms") {

    forAll(genTables) { (t: Tables) =>

      val actual = runTest(t) { repo =>

        t.observations.map { case (oid, om) =>
          val atoms =
            om
            .config
            .flatMap(_.dereference[State[Tables, *], Tables](TableState).runA(t).value)
            .map {
              case InstrumentConfigModel.GmosNorth(_, _, sci) =>
                repo.executionEvent.selectRemainingAtoms(oid, sci)
              case InstrumentConfigModel.GmosSouth(_, _, sci) =>
                repo.executionEvent.selectRemainingAtoms(oid, sci)
              case _                                          =>
                IO(List.empty)
            }.sequence.map(_.toList.flatten.map(_.id))
        }

      }

    }

  }
   */

}

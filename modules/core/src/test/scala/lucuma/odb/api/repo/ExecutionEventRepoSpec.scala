// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.core.model.{Atom, Observation, Step}
import lucuma.odb.api.model.{AtomModel, ExecutionEventModel}
import lucuma.odb.api.model.ExecutionEventModel.{StepEvent, StepStageType}
import lucuma.odb.api.model.ExecutionEventModel.StepStageType.EndStep
import lucuma.odb.api.model.SequenceModel.SequenceType.Science
import cats.syntax.all._
import cats.implicits.catsKernelOrderingForOrder
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

  property("selectExecutedStepsForObservation is ordered") {

    forAll(genTables) { (t: Tables) =>

      val actual = runTest(t) { repo =>
        t.observations.keys.toList.traverse { oid =>
          repo
            .executionEvent
            .selectExecutedStepsForObservation(oid)
        }
      }

      val sorted = actual.map(_.sortBy(e => (e.endTime, e.stepId)))

      assertEquals(actual, sorted)
    }

  }

  property("selectRemainingAtoms") {

    forAll(genTables) { (t: Tables) =>

      val isExecutedStep: Set[Step.Id] =
        t.executionEvents.values.collect {
          case ExecutionEventModel.StepEvent(_, _, _, _, sid, Science, EndStep) => sid
        }.toSet

      def isExecutedAtom(a: AtomModel[Step.Id]): Boolean =
        a.steps.forall(isExecutedStep)

      val remaining: Map[Observation.Id, Set[Atom.Id]] =
        runTest(t) { repo =>
          t.observations.keys.toList.traverse { oid =>
            repo
              .executionEvent
              .selectRemainingAtoms(oid, Science)
              .map(_.map(_.id).toSet)
              .tupleLeft(oid)
          }
        }.toMap

      t.observations.view.values.forall { om =>
        om.config.toList.flatMap(_.science.atoms).forall { aid =>
          remaining.get(om.id).forall(_.contains(aid)) === !isExecutedAtom(t.atoms(aid))
        }
      }

    }

  }
}

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.syntax.eq._
import lucuma.core.enums.DatasetQaState
import lucuma.core.util.arb.ArbEnumerated
import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

/**
 *
 */
class StepQaStateSuite extends ScalaCheckSuite {

  import ArbEnumerated._

  property("one bad apple spoils the bunch") {
    forAll { (lst: List[Option[DatasetQaState]]) =>
      StepQaState.rollup(lst) match {
        case Some(StepQaState.Fail) => lst.exists(_.exists(_ =!= DatasetQaState.Pass))
        case _                      => true
      }
    }
  }

  property("to pass all datasets must pass") {
    forAll { (lst: List[Option[DatasetQaState]]) =>
      StepQaState.rollup(lst) match {
        case Some(StepQaState.Pass) => lst.nonEmpty && lst.forall(_.exists(_ === DatasetQaState.Pass))
        case _                      => true
      }
    }
  }

  property("undefined when it is unclear") {
    forAll { (lst: List[Option[DatasetQaState]]) =>
      StepQaState.rollup(lst) match {
        case None => lst.isEmpty || lst.exists(_.isEmpty)
        case _    => true
      }
    }
  }
}

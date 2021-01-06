// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbManualSequence {

  import ArbStepModel._

  implicit def arbManualSequence[S: Arbitrary, D: Arbitrary]: Arbitrary[ManualSequence[S, D]] =
    Arbitrary {
      for {
        st <- arbitrary[S]
        aq <- arbitrary[List[StepModel[D]]]
        sc <- arbitrary[List[StepModel[D]]]
      } yield ManualSequence(st, aq, sc)
    }

  implicit def cogManualSequence[S: Cogen, D: Cogen]: Cogen[ManualSequence[S, D]] =
    Cogen[(
      S,
      List[StepModel[D]],
      List[StepModel[D]]
    )].contramap { in => (
      in.static,
      in.acquisition,
      in.science
    )}

  implicit def arbCreateManualSequence[S: Arbitrary, D: Arbitrary]: Arbitrary[ManualSequence.Create[S, D]] =
    Arbitrary {
      for {
        st <- arbitrary[S]
        aq <- arbitrary[List[StepModel.CreateStep[D]]]
        sc <- arbitrary[List[StepModel.CreateStep[D]]]
      } yield ManualSequence.Create(st, aq, sc)
    }

  implicit def cogCreateManualSequence[S: Cogen, D: Cogen]: Cogen[ManualSequence.Create[S, D]] =
    Cogen[(
      S,
      List[StepModel.CreateStep[D]],
      List[StepModel.CreateStep[D]]
    )].contramap { in => (
      in.static,
      in.acquisition,
      in.science
    )}

}

object ArbManualSequence extends ArbManualSequence

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.core.arb.ArbTime
import lucuma.core.util.arb.ArbGid
import lucuma.core.model.Observation
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

import java.time.Instant

trait ArbStepRecord {

  import ArbGid._
  import ArbStepModel._
  import ArbTime._
  import ArbUid._

  implicit def arbStepRecord[D: Arbitrary]: Arbitrary[StepRecord[D]] =
    Arbitrary {
      for {
        t <- arbitrary[Instant]
        c <- arbitrary[StepConfig[D]]
      } yield StepRecord[D](t, c)
    }

  implicit def cogStepRecord[D: Cogen]: Cogen[StepRecord[D]] =
    Cogen[(
      Instant,
      StepConfig[D]
    )].contramap { in => (
      in.created,
      in.stepConfig
    )}

  implicit def arbStepRecordInput[D: Arbitrary]: Arbitrary[StepRecord.Input[D]] =
    Arbitrary {
      for {
        o <- arbitrary[Observation.Id]
        v <- arbitrary[Visit.Id]
        c <- arbitrary[StepConfig.CreateStepConfig[D]]
      } yield StepRecord.Input(o, v, c)
    }

  implicit def cogStepRecordInput[D: Cogen]: Cogen[StepRecord.Input[D]] =
    Cogen[(
      Observation.Id,
      Visit.Id,
      StepConfig.CreateStepConfig[D]
    )].contramap { in => (
      in.observationId,
      in.visitId,
      in.stepConfig
    )}

}

object ArbStepRecord extends ArbStepRecord

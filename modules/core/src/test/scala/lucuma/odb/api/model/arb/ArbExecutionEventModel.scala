// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import ExecutionEventModel._
import lucuma.core.arb.ArbTime
import lucuma.core.model.{Observation, Step}
import lucuma.core.util.arb.{ArbEnumerated, ArbGid}
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

import java.time.Instant


trait ArbExecutionEventModel {

  import ArbEnumerated._
  import ArbGid._
  import ArbTime._

  implicit val arbSequenceEvent: Arbitrary[SequenceEvent] =
    Arbitrary {
      for {
        id  <- arbitrary[ExecutionEvent.Id]
        oid <- arbitrary[Observation.Id]
        gen <- arbitrary[Instant]
        rec <- arbitrary[Instant]
        cmd <- arbitrary[SequenceCommandType]
      } yield SequenceEvent(id, oid, gen, rec, cmd)
    }

  implicit val cogSequenceEvent: Cogen[SequenceEvent] =
    Cogen[(
      ExecutionEvent.Id,
      Observation.Id,
      Instant,
      Instant,
      SequenceCommandType
    )].contramap { a => (
      a.id,
      a.observationId,
      a.generated,
      a.received,
      a.command
    )}

  implicit val arbStepEvent: Arbitrary[StepEvent] =
    Arbitrary {
      for {
        id  <- arbitrary[ExecutionEvent.Id]
        oid <- arbitrary[Observation.Id]
        gen <- arbitrary[Instant]
        rec <- arbitrary[Instant]
        sid <- arbitrary[Step.Id]
        tpe <- arbitrary[SequenceModel.SequenceType]
        sge <- arbitrary[StepStageType]
      } yield StepEvent(id, oid, gen, rec, sid, tpe, sge)
    }

  implicit val cogStepEvent: Cogen[StepEvent] =
    Cogen[(
      ExecutionEvent.Id,
      Observation.Id,
      Instant,
      Instant,
      Step.Id,
      SequenceModel.SequenceType,
      StepStageType
    )].contramap { a => (
      a.id,
      a.observationId,
      a.generated,
      a.received,
      a.stepId,
      a.sequenceType,
      a.stage
    )}

}

object ArbExecutionEventModel extends ArbExecutionEventModel

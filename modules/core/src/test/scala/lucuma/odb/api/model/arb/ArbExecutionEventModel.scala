// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import ExecutionEventModel._
import lucuma.core.arb.ArbTime
import lucuma.core.model.{ExecutionEvent, Observation}
import lucuma.core.util.arb.{ArbEnumerated, ArbGid}
import cats.syntax.all._
import eu.timepit.refined.types.all.PosInt
import eu.timepit.refined.scalacheck.numeric._
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

import java.time.Instant
import java.util.UUID


trait ArbExecutionEventModel {

  import ArbEnumerated._
  import ArbDatasetFilename._
  import ArbGid._
  import ArbStepModel.{arbStepId, cogStepId}
  import ArbTime._

  implicit val arbVisitId: Arbitrary[Visit.Id] =
    Arbitrary {
      arbitrary[UUID].map(Visit.Id.fromUuid)
    }

  implicit val cogVisitId: Cogen[Visit.Id] =
    Cogen[UUID].contramap(_.toUuid)

  implicit val arbSequenceEvent: Arbitrary[SequenceEvent] =
    Arbitrary {
      for {
        id  <- arbitrary[ExecutionEvent.Id]
        oid <- arbitrary[Observation.Id]
        vid <- arbitrary[Visit.Id]
        rec <- arbitrary[Instant]
        cmd <- arbitrary[SequenceCommandType]
      } yield SequenceEvent(id, oid, vid, rec, cmd)
    }

  implicit val cogSequenceEvent: Cogen[SequenceEvent] =
    Cogen[(
      ExecutionEvent.Id,
      Observation.Id,
      Visit.Id,
      Instant,
      SequenceCommandType
    )].contramap { a => (
      a.id,
      a.observationId,
      a.visitId,
      a.received,
      a.command
    )}

  def arbSequenceEventAdd(
    oid: Observation.Id,
    vid: Visit.Id
  ): Arbitrary[SequenceEvent.Add] =
    Arbitrary {
      arbitrary[SequenceCommandType].map { cmd =>
        SequenceEvent.Add(oid, vid, SequenceEvent.Payload(cmd))
      }
    }

  implicit val arbStepEvent: Arbitrary[StepEvent] =
    Arbitrary {
      for {
        id  <- arbitrary[ExecutionEvent.Id]
        oid <- arbitrary[Observation.Id]
        vid <- arbitrary[Visit.Id]
        sid <- arbitrary[Step.Id]
        rec <- arbitrary[Instant]
        tpe <- arbitrary[SequenceModel.SequenceType]
        sge <- arbitrary[StepStageType]
      } yield StepEvent(id, oid, vid, sid, rec, tpe, sge)
    }

  implicit val cogStepEvent: Cogen[StepEvent] =
    Cogen[(
      ExecutionEvent.Id,
      Observation.Id,
      Visit.Id,
      Step.Id,
      Instant,
      SequenceModel.SequenceType,
      StepStageType
    )].contramap { a => (
      a.id,
      a.observationId,
      a.visitId,
      a.stepId,
      a.received,
      a.sequenceType,
      a.stage
    )}

  def arbStepEventAdd(
    oid: Observation.Id,
    vid: Visit.Id,
    sid: Step.Id,
    stp: SequenceModel.SequenceType
  ): Arbitrary[StepEvent.Add] =
    Arbitrary {
      arbitrary[StepStageType].map { cmd =>
        StepEvent.Add(
          oid,
          vid,
          sid,
          StepEvent.Payload(stp, cmd)
        )
      }
    }

  implicit val arbDatasetEvent: Arbitrary[DatasetEvent] =
    Arbitrary {
      for {
        id  <- arbitrary[ExecutionEvent.Id]
        oid <- arbitrary[Observation.Id]
        vid <- arbitrary[Visit.Id]
        sid <- arbitrary[Step.Id]
        rec <- arbitrary[Instant]
        idx <- arbitrary[PosInt]
        fnm <- arbitrary[Option[DatasetFilename]]
        sge <- arbitrary[DatasetStageType]
      } yield DatasetEvent(id, oid, vid, sid, rec, idx,  sge, fnm)
    }

  implicit val cogDatasetEvent: Cogen[DatasetEvent] =
    Cogen[(
      ExecutionEvent.Id,
      Observation.Id,
      Visit.Id,
      Step.Id,
      Instant,
      Int,
      Option[DatasetFilename],
      DatasetStageType
    )].contramap { in => (
      in.id,
      in.observationId,
      in.visitId,
      in.stepId,
      in.received,
      in.datasetIndex.value,
      in.filename,
      in.stageType
    )}

  def arbDatasetEventAdd(
    oid: Observation.Id,
    vid: Visit.Id,
    sid: Step.Id
  ): Arbitrary[DatasetEvent.Add] =
    Arbitrary {
      for {
        fnm <- arbitrary[Option[DatasetFilename]]
        cmd <- arbitrary[DatasetStageType]
      } yield DatasetEvent.Add(
        oid,
        vid,
        sid,
        PosInt.MinValue,
        DatasetEvent.Payload(cmd, fnm)
      )
    }

  implicit val arbExecutionEvent: Arbitrary[ExecutionEventModel] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[SequenceEvent],
        arbitrary[StepEvent],
        arbitrary[DatasetEvent]
      )
    }

  implicit val cogExecutionEventModel: Cogen[ExecutionEventModel] =
    Cogen[(
      Option[SequenceEvent],
      Option[StepEvent],
      Option[DatasetEvent]
    )].contramap {
      case e: SequenceEvent => (e.some, none, none)
      case e: StepEvent     => (none, e.some, none)
      case e: DatasetEvent  => (none, none, e.some)
    }

}

object ArbExecutionEventModel extends ArbExecutionEventModel

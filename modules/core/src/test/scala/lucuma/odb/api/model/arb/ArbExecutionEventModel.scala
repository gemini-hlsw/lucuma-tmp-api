// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import ExecutionEventModel._

import lucuma.core.arb.ArbTime
import lucuma.core.model.{ExecutionEvent, Observation, Step}
import lucuma.core.util.arb.{ArbEnumerated, ArbGid}

import cats.syntax.all._
import eu.timepit.refined.types.all.PosInt
import eu.timepit.refined.scalacheck.numeric._
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

import java.time.Instant


trait ArbExecutionEventModel {

  import ArbEnumerated._
  import ArbDatasetFilename._
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

  def arbSequenceEventAdd(
    oid: Observation.Id
  ): Arbitrary[SequenceEvent.Add] =
    Arbitrary {
      for {
        gen <- arbitrary[Instant]
        cmd <- arbitrary[SequenceCommandType]
      } yield SequenceEvent.Add(
        Option.empty[ExecutionEvent.Id],
        oid,
        gen,
        cmd
      )
    }

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

  def arbStepEventAdd(
    oid: Observation.Id,
    sid: Step.Id,
    stp: SequenceModel.SequenceType
  ): Arbitrary[StepEvent.Add] =
    Arbitrary {
      for {
        gen <- arbitrary[Instant]
        cmd <- arbitrary[StepStageType]
      } yield StepEvent.Add(
        Option.empty[ExecutionEvent.Id],
        oid,
        gen,
        sid,
        stp,
        cmd
      )
    }

  implicit val arbDatasetEvent: Arbitrary[DatasetEvent] =
    Arbitrary {
      for {
        id  <- arbitrary[ExecutionEvent.Id]
        oid <- arbitrary[Observation.Id]
        gen <- arbitrary[Instant]
        rec <- arbitrary[Instant]
        sid <- arbitrary[Step.Id]
        idx <- arbitrary[PosInt]
        fnm <- arbitrary[Option[DatasetFilename]]
        sge <- arbitrary[DatasetStageType]
      } yield DatasetEvent(id, oid, gen, rec, sid, idx, fnm, sge)
    }

  implicit val cogDatasetEvent: Cogen[DatasetEvent] =
    Cogen[(
      ExecutionEvent.Id,
      Observation.Id,
      Instant,
      Instant,
      Step.Id,
      Int,
      Option[DatasetFilename],
      DatasetStageType
    )].contramap { in => (
      in.id,
      in.observationId,
      in.generated,
      in.received,
      in.stepId,
      in.datasetIndex.value,
      in.filename,
      in.stageType
    )}

  def arbDatasetEventAdd(
    oid: Observation.Id,
    sid: Step.Id
  ): Arbitrary[DatasetEvent.Add] =
    Arbitrary {
      for {
        gen <- arbitrary[Instant]
        fnm <- arbitrary[Option[DatasetFilename]]
        cmd <- arbitrary[DatasetStageType]
      } yield DatasetEvent.Add(
        Option.empty[ExecutionEvent.Id],
        oid,
        gen,
        sid,
        PosInt.MinValue,
        fnm,
        cmd
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

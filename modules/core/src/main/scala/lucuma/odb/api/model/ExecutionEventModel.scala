// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.data.StateT
import cats.{Eq, Order, Show}
import cats.syntax.eq._
import cats.syntax.either._
import eu.timepit.refined.types.numeric._
import org.typelevel.cats.time.instances.instant._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.refined._
import eu.timepit.refined.auto._
import lucuma.core.model.{ExecutionEvent, Observation}
import lucuma.core.util.Enumerated
import lucuma.odb.api.model.syntax.lens._

import java.time.Instant

/**
 * Shared interface for all execution events: sequence, step and dataset.
 */
sealed trait ExecutionEventModel {

  def id:            ExecutionEvent.Id

  def observationId: Observation.Id

  def visitId:       Visit.Id

  def received:      Instant

}

object ExecutionEventModel {

  implicit val EqExecutionEventModel: Eq[ExecutionEventModel] =
    Eq.instance {
      case (e0: SequenceEvent, e1: SequenceEvent) => e0 === e1
      case (e0: StepEvent,     e1: StepEvent)     => e0 === e1
      case (e0: DatasetEvent,  e1: DatasetEvent)  => e0 === e1
      case _                                      => false
    }

  // Sequence-level Events ----------------------------------------------------

  sealed abstract class SequenceCommandType(
    val tag:       String,
    val shortName: String
  ) extends Product with Serializable


  object SequenceCommandType {

    case object Abort    extends SequenceCommandType("ABORT", "Abort")
    case object Continue extends SequenceCommandType("CONTINUE", "Continue")
    case object Pause    extends SequenceCommandType("PAUSE", "Pause")
    case object Slew     extends SequenceCommandType("SLEW", "Slew")
    case object Start    extends SequenceCommandType("START", "Start")
    case object Stop     extends SequenceCommandType("STOP", "Stop")

    val all: List[SequenceCommandType] =
      List(
        Abort,
        Continue,
        Pause,
        Slew,
        Start,
        Stop
      )

    def fromTag(s: String): Option[SequenceCommandType] =
      all.find(_.tag === s)

    def unsafeFromTag(s: String): SequenceCommandType =
      fromTag(s).getOrElse(throw new NoSuchElementException(s"SequenceCommandType: Invalid tag: '$s'"))

    implicit val EnumeratedSequenceCommandType: Enumerated[SequenceCommandType] =
      new Enumerated[SequenceCommandType] {
        def all: List[SequenceCommandType] = SequenceCommandType.all
        def tag(a: SequenceCommandType): String = a.tag
        override def unsafeFromTag(s: String): SequenceCommandType =
          SequenceCommandType.unsafeFromTag(s)
      }

  }

  final case class SequenceEvent(
    id:            ExecutionEvent.Id,
    observationId: Observation.Id,
    visitId:       Visit.Id,
    received:      Instant,
    command:       SequenceCommandType
  ) extends ExecutionEventModel

  object SequenceEvent {

    implicit val OrderSequenceEvent: Order[SequenceEvent] = {
      Order.by { a => (
        a.id,
        a.observationId,
        a.visitId,
        a.command,
        a.received
      )}
    }

    final case class Add(
      observationId: Observation.Id,
      visitId:       Visit.Id,
      command:       SequenceCommandType
    ) {

      def add(
        received: Instant
      ): StateT[EitherInput, Database, SequenceEvent] =

        for {
          i <- Database.executionEvent.cycleNextUnused
          _ <- Database.observation.lookup(observationId)
          _ <- VisitRecords.visitAt(observationId, visitId)
          e  = SequenceEvent(i, observationId, visitId, received, command)
          _ <- Database.executionEvent.saveNew(i, e)
        } yield e

    }

    object Add {

      implicit val DecoderAdd: Decoder[Add] =
        deriveDecoder[Add]

      implicit val OrderAdd: Order[Add] =
        Order.by { a => (
          a.observationId,
          a.visitId,
          a.command
        )}

    }

  }


  // Step-level Events --------------------------------------------------------

  sealed abstract class StepStageType(
    val tag:       String,
    val shortName: String
  ) extends Product with Serializable

  object StepStageType {
    case object EndConfigure   extends StepStageType("END_CONFIGURE", "EndConfigure")
    case object EndObserve     extends StepStageType("END_OBSERVE", "EndObserve")
    case object EndStep        extends StepStageType("END_STEP", "EndStep")
    case object StartConfigure extends StepStageType("START_CONFIGURE", "StartConfigure")
    case object StartObserve   extends StepStageType("START_OBSERVE", "StartObserve")
    case object StartStep      extends StepStageType("START_STEP", "StartStep")

    val all: List[StepStageType] =
      List(
        EndConfigure,
        EndObserve,
        EndStep,
        StartConfigure,
        StartObserve,
        StartStep
      )

    def fromTag(s: String): Option[StepStageType] =
      all.find(_.tag === s)

    def unsafeFromTag(s: String): StepStageType =
      fromTag(s).getOrElse(throw new NoSuchElementException(s"StepStageType: Invalid tag: '$s'"))

    implicit val EnumeratedStepStageType: Enumerated[StepStageType] =
      new Enumerated[StepStageType] {
        override def all: List[StepStageType] = StepStageType.all
        override def tag(a: StepStageType): String = a.tag
        override def unsafeFromTag(s: String): StepStageType = StepStageType.unsafeFromTag(s)
      }

  }

  final case class StepEvent(
    id:            ExecutionEvent.Id,
    observationId: Observation.Id,
    visitId:       Visit.Id,
    stepId:        Step.Id,
    received:      Instant,

    sequenceType:  SequenceModel.SequenceType,
    stage:         StepStageType

  ) extends ExecutionEventModel

  object StepEvent {

    implicit val OrderStepEvent: Order[StepEvent] =
      Order.by { a => (
        a.id,
        a.observationId,
        a.visitId,
        a.stepId,
        a.received,
        a.sequenceType,
        a.stage,
      )}

    final case class Add(
      observationId: Observation.Id,
      visitId:       Visit.Id,
      stepId:        Step.Id,

      sequenceType:  SequenceModel.SequenceType,
      stage:         StepStageType
    ) {

      def add(
        received:     Instant
      ): StateT[EitherInput, Database, StepEvent] =

        for {
          i <- Database.executionEvent.cycleNextUnused
          _ <- Database.observation.lookup(observationId)
          _ <- VisitRecords.stepAt(observationId, visitId, stepId)
          e  =
            StepEvent(
              i,
              observationId,
              visitId,
              stepId,
              received,
              sequenceType,
              stage
            )
          _ <- Database.executionEvent.saveNew(i, e)
        } yield e

    }

    object Add {

      implicit val DecoderAdd: Decoder[Add] =
        deriveDecoder[Add]

      implicit val OrderAdd: Order[Add] =
        Order.by { a => (
          a.observationId,
          a.visitId,
          a.stepId,
          a.sequenceType,
          a.stage
        )}

    }

  }

  // Dataset-level Events -----------------------------------------------------

  sealed abstract class DatasetStageType(
    val tag:       String,
    val shortName: String
  ) extends Product with Serializable

  object DatasetStageType {
    case object EndObserve    extends DatasetStageType("END_OBSERVE", "EndObserve")
    case object EndReadout    extends DatasetStageType("END_READOUT", "EndReadout")
    case object EndWrite      extends DatasetStageType("END_WRITE", "EndWrite")
    case object StartObserve  extends DatasetStageType("START_OBSERVE", "StartObserve")
    case object StartReadout  extends DatasetStageType("START_READOUT", "StartReadout")
    case object StartWrite    extends DatasetStageType("START_WRITE", "StartWrite")

    val all: List[DatasetStageType] =
      List(
        EndObserve,
        EndReadout,
        EndWrite,
        StartObserve,
        StartReadout,
        StartWrite
      )

    def fromTag(s: String): Option[DatasetStageType] =
      all.find(_.tag === s)

    def unsafeFromTag(s: String): DatasetStageType =
      fromTag(s).getOrElse(throw new NoSuchElementException(s"DatasetStageType: Invalid tag: '$s'"))

    implicit val EnumeratedDatasetStageType: Enumerated[DatasetStageType] =
      new Enumerated[DatasetStageType] {
        override def all: List[DatasetStageType] = DatasetStageType.all
        override def tag(a: DatasetStageType): String = a.tag
        override def unsafeFromTag(s: String): DatasetStageType = DatasetStageType.unsafeFromTag(s)
      }

  }

  final case class DatasetEvent(
    id:            ExecutionEvent.Id,
    observationId: Observation.Id,
    visitId:       Visit.Id,
    stepId:        Step.Id,
    received:      Instant,

    datasetIndex:  PosInt,
    filename:      Option[DatasetFilename],
    stageType:     DatasetStageType
  ) extends ExecutionEventModel {

    def datasetId: DatasetModel.Id =
      DatasetModel.Id(stepId, datasetIndex)

    def toDataset: Option[DatasetModel] =
      filename.map { fn =>
        DatasetModel(datasetId, DatasetModel.Dataset(observationId, fn, None))
      }

  }

  object DatasetEvent {

    implicit val OrderDatasetEvent: Order[DatasetEvent] =
      Order.by { a => (
        a.id,
        a.observationId,
        a.visitId,
        a.stepId,
        a.datasetIndex.value,
        a.filename,
        a.stageType,
        a.received
      )}

    final case class Add(
      observationId: Observation.Id,
      visitId:       Visit.Id,
      stepId:        Step.Id,
      datasetIndex:  PosInt,
      filename:      Option[DatasetFilename],
      stage:         DatasetStageType
    ) {

      private def recordDataset(
        e: DatasetEvent
      ): StateT[EitherInput, Database, Unit] = {

        val empty: StateT[EitherInput, Database, Unit] =
          StateT.empty

        def doRecord(
          dset: DatasetModel
        ): StateT[EitherInput, Database, Unit] =
          for {
            d  <- Database.datasets.st.map(_.get(dset.id))
            _  <- d.fold(Database.datasets.mod_(_.updated(dset))) { existing =>
              if (existing.dataset.filename === dset.dataset.filename) empty
              else StateT.setF(InputError(s"Dataset ${Show[DatasetModel.Id].show(dset.id)} has recorded file ${existing.dataset.filename} but event has ${dset.dataset.filename}").leftNec)
            }
          } yield ()

        e.toDataset.fold(empty)(doRecord)
      }


      def add(
        received: Instant
      ): StateT[EitherInput, Database, DatasetEvent] =

        for {
          i <- Database.executionEvent.cycleNextUnused
          _ <- Database.observation.lookup(observationId)
          _ <- VisitRecords.stepAt(observationId, visitId, stepId)
          e  =
            DatasetEvent(
              i,
              observationId,
              visitId,
              stepId,
              received,
              datasetIndex,
              filename,
              stage
            )
          _ <- recordDataset(e)
          _ <- Database.executionEvent.saveNew(i, e)
        } yield e

    }

    object Add {

      implicit val DecoderAdd: Decoder[Add] =
        deriveDecoder[Add]

      implicit val OrderAdd: Order[Add] =
        Order.by { a => (
          a.observationId,
          a.visitId,
          a.stepId,
          a.datasetIndex.value,
          a.filename,
          a.stage
        )}
    }

  }

}

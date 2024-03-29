// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.data.StateT
import cats.{Eq, Order, Show}
import cats.syntax.eq._
import cats.syntax.either._
import org.typelevel.cats.time.instances.instant._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import lucuma.core.model.{ExecutionEvent, Observation}
import lucuma.core.util.Enumerated
import lucuma.odb.api.model.syntax.lens._

import java.time.Instant

/**
 * Shared interface for all execution events: sequence, step and dataset.
 */
sealed trait ExecutionEventModel {

  def id:            ExecutionEvent.Id

  def visitId:       Visit.Id

  def observationId: Observation.Id

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
    visitId:       Visit.Id,
    received:      Instant,
    location:      SequenceEvent.Location,
    payload:       SequenceEvent.Payload
  ) extends ExecutionEventModel {

    override def observationId: Observation.Id =
      location.observationId

  }

  object SequenceEvent {

    implicit val OrderSequenceEvent: Order[SequenceEvent] = {
      Order.by { a => (
        a.id,
        a.visitId,
        a.received,
        a.location,
        a.payload
      )}
    }

    final case class Location(
      observationId: Observation.Id
    )

    object Location {
      implicit val DecoderLocation: Decoder[Location] =
        deriveDecoder[Location]

      implicit val OrderLocation: Order[Location] =
        Order.by(_.observationId)
    }

    final case class Payload(
      command: SequenceCommandType
    )

    object Payload {
      implicit val DecoderPayload: Decoder[Payload] =
        deriveDecoder[Payload]

      implicit val OrderPayload: Order[Payload] =
        Order.by(_.command)
    }

    final case class Add(
      visitId:  Visit.Id,
      location: Location,
      payload:  Payload
    ) {

      def add(
        received: Instant
      ): StateT[EitherInput, Database, SequenceEvent] =

        for {
          i <- Database.executionEvent.cycleNextUnused
          _ <- Database.observation.lookup(location.observationId)
          _ <- VisitRecords.visitAt(location.observationId, visitId)
          e  = SequenceEvent(i, visitId, received, location, payload)
          _ <- Database.executionEvent.saveNew(i, e)
        } yield e

    }

    object Add {

      implicit val DecoderAdd: Decoder[Add] =
        deriveDecoder[Add]

      implicit val OrderAdd: Order[Add] =
        Order.by { a => (
          a.visitId,
          a.location,
          a.payload
        )}

    }

    final case class Result(
      event: SequenceEvent
    )

    object Result {

      implicit val OrderResult: Order[Result] =
        Order.by(_.event)

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
    id:       ExecutionEvent.Id,
    visitId:  Visit.Id,
    received: Instant,
    location: StepEvent.Location,
    payload:  StepEvent.Payload

  ) extends ExecutionEventModel {

    override def observationId: Observation.Id =
      location.observationId

  }

  object StepEvent {

    implicit val OrderStepEvent: Order[StepEvent] =
      Order.by { a => (
        a.id,
        a.visitId,
        a.received,
        a.location,
        a.payload
      )}

    final case class Location(
      observationId: Observation.Id,
      stepId:        Step.Id
    )

    object Location {
      implicit val DecoderLocation: Decoder[Location] =
        deriveDecoder[Location]

      implicit val OrderLocation: Order[Location] =
        Order.by { a => (
          a.observationId,
          a.stepId
        )}
    }

    final case class Payload(
      sequenceType: SequenceModel.SequenceType,
      stepStage:    StepStageType
    )

    object Payload {
      implicit val DecoderPayload: Decoder[Payload] =
        deriveDecoder[Payload]

      implicit val OrderPayload: Order[Payload] =
        Order.by { a => (
          a.sequenceType,
          a.stepStage
        )}
    }

    final case class Add(
      visitId:  Visit.Id,
      location: Location,
      payload:  Payload
    ) {

      def add(
        received:     Instant
      ): StateT[EitherInput, Database, StepEvent] =

        for {
          i <- Database.executionEvent.cycleNextUnused
          _ <- Database.observation.lookup(location.observationId)
          _ <- VisitRecords.stepAt(location.observationId, visitId, location.stepId)
          e  =
            StepEvent(
              i,
              visitId,
              received,
              location,
              payload
            )
          _ <- Database.executionEvent.saveNew(i, e)
        } yield e

    }

    object Add {

      implicit val DecoderAdd: Decoder[Add] =
        deriveDecoder[Add]

      implicit val OrderAdd: Order[Add] =
        Order.by { a => (
          a.visitId,
          a.location,
          a.payload
        )}

    }

    final case class Result(
      event: StepEvent
    )

    object Result {

      implicit val OrderResult: Order[Result] =
        Order.by(_.event)

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
    id:       ExecutionEvent.Id,
    visitId:  Visit.Id,
    received: Instant,
    location: DatasetModel.Id,
    payload:  DatasetEvent.Payload
  ) extends ExecutionEventModel {

    override def observationId: Observation.Id =
      location.observationId

    def toDataset: Option[DatasetModel] =
      payload.filename.map { fn =>
        DatasetModel(location, DatasetModel.Dataset(fn, None))
      }

  }

  object DatasetEvent {

    implicit val OrderDatasetEvent: Order[DatasetEvent] =
      Order.by { a => (
        a.id,
        a.visitId,
        a.location,
        a.payload,
        a.received
      )}

    final case class Payload(
      datasetStage: DatasetStageType,
      filename:     Option[DatasetFilename]
    )

    object Payload {
      implicit val DecoderPayload: Decoder[Payload] =
        deriveDecoder[Payload]

      implicit val OrderPayload: Order[Payload] =
        Order.by { a => (
          a.datasetStage,
          a.filename
        )}
    }

    final case class Add(
      visitId:  Visit.Id,
      location: DatasetModel.Id,
      payload:  Payload
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
            d  <- Database.datasets.st.map(_.select(dset.id))
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
          _ <- Database.observation.lookup(location.observationId)
          _ <- VisitRecords.stepAt(location.observationId, visitId, location.stepId)
          e  =
            DatasetEvent(
              i,
              visitId,
              received,
              location,
              payload
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
          a.visitId,
          a.location,
          a.payload
        )}
    }

    final case class Result(
      event: DatasetEvent
    )

    object Result {

      implicit val OrderResult: Order[Result] =
        Order.by(_.event)

    }

  }

}

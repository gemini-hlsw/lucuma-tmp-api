// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{DatabaseState, ExecutionEvent, ExecutionEventModel, InputError, ValidatedInput}
import lucuma.odb.api.model.ExecutionEventModel.{DatasetEvent, SequenceEvent, StepEvent}
import lucuma.core.model.Observation

import cats.data.{EitherT, State}
import cats.syntax.all._
import cats.effect.Sync
import cats.effect.concurrent.Ref

import java.time.Instant


sealed trait ExecutionEventRepo[F[_]] {

  def selectEvent(
    eid: ExecutionEvent.Id
  ): F[Option[ExecutionEventModel]]

  def selectEventsForObservation(
    oid:      Observation.Id,
    count:    Int,
    afterGid: Option[ExecutionEvent.Id] = None
  ): F[ResultPage[ExecutionEventModel]]

  def insertSequenceEvent(
    event: SequenceEvent.Create
  ): F[SequenceEvent]

  def insertStepEvent(
    event: StepEvent.Create
  ): F[StepEvent]

  def insertDatasetEvent(
    event: DatasetEvent.Create
  ): F[DatasetEvent]

}

object ExecutionEventRepo {

  def create[F[_]: Sync](
    tablesRef: Ref[F, Tables]
  ): ExecutionEventRepo[F] =
    new ExecutionEventRepo[F] {

      override def selectEvent(
        eid: ExecutionEvent.Id
      ): F[Option[ExecutionEventModel]] =
        tablesRef.get.map(Tables.executionEvent(eid).get)

      override def selectEventsForObservation(
        oid:      Observation.Id,
        count:    Int,
        afterGid: Option[ExecutionEvent.Id]
      ): F[ResultPage[ExecutionEventModel]] =
        tablesRef.get.map { tables =>

          ResultPage.select[ExecutionEvent.Id, ExecutionEventModel](
            count,
            afterGid,
            tables.executionEvents.keySet,
            tables.executionEvents.apply,
            _.observationId === oid
          )

        }

      private def received: F[Instant] =
        Sync[F].delay(Instant.now)

      private def runState[T](
        s: State[Tables, ValidatedInput[T]]
      ): F[T] =
        EitherT(
          tablesRef.modify { tables =>
            val (tablesʹ, e) = s.run(tables).value
            e.fold(
              err => (tables, InputError.Exception(err).asLeft),
              evt => (tablesʹ, evt.asRight)
            )
          }
        ).rethrowT

      private def insertEvent[A](
        f: (DatabaseState[Tables], Instant) => State[Tables, ValidatedInput[A]]
      ): F[A] =
        for {
          w <- received
          e <- runState(f(TableState, w))
        } yield e

      override def insertSequenceEvent(
        event: SequenceEvent.Create
      ): F[SequenceEvent] =
        insertEvent(event.create[State[Tables, *], Tables])

      override def insertStepEvent(
        event: StepEvent.Create
      ): F[StepEvent] =
        insertEvent(event.create[State[Tables, *], Tables])

      override def insertDatasetEvent(
        event: DatasetEvent.Create
      ): F[DatasetEvent] =
        insertEvent(event.create[State[Tables, *], Tables])

    }


}

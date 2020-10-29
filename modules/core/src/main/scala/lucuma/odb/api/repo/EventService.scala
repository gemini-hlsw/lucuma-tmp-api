// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.FlatMap
import lucuma.odb.api.model.Event
import cats.effect.Concurrent
import cats.implicits._
import cats.effect.concurrent.Ref
import fs2.Stream
import fs2.concurrent.Topic

/**
 *
 */
final class EventService[F[_]](
  topic:     Topic[F, Event],
  refTables: Ref[F, Tables]
) {

  def subscribe: Stream[F, Event] =
    topic.subscribe(100)

  def publish(f: Long => Event)(implicit F: FlatMap[F]): F[Unit] =
    for {
      n <- refTables.modifyState(TableState.nextEventId)
      _ <- topic.publish1(f(n))
    } yield ()

}

object EventService {

  def apply[F[_]: Concurrent](r: Ref[F, Tables]): F[EventService[F]] =
    Topic(Event.initialize).map(t => new EventService(t, r))

}

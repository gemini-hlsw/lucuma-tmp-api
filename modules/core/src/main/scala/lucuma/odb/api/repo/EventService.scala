// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.Event
import cats.effect.{Concurrent, Ref}
import cats.implicits._


import fs2.Stream
import fs2.concurrent.Topic

/**
 *
 */
final class EventService[F[_]](
  topic:     Topic[F, Event],
  refTables: Ref[F, Tables]
)(implicit F: Concurrent[F]) {

  def subscribe: Stream[F, Event] =
    topic.subscribe(100)

  def publish(f: Long => Event): F[Unit] =
    for {
      n <- refTables.modifyState(TableState.nextEventId)
      _ <- topic.publish1(f(n))
    } yield ()

}

object EventService {

  def apply[F[_]: Concurrent](r: Ref[F, Tables]): F[EventService[F]] =
    Topic[F, Event].map(t => new EventService[F](t, r))

}

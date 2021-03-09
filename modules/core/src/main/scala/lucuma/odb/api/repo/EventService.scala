// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.Event
import cats.effect.Concurrent
import cats.implicits._
import cats.effect.concurrent.Ref
import org.typelevel.log4cats.Logger


import fs2.Stream
import fs2.concurrent.Topic

/**
 *
 */
final class EventService[F[_]: Logger](
  topic:     Topic[F, Event],
  refTables: Ref[F, Tables]
)(implicit F: Concurrent[F]) {

  def subscribe: Stream[F, Event] =
    topic.subscribeSize(100)
      .evalTap { case (e, i) =>
        Logger[F].info(s"subscription event ($i still queued): $e")
      }
      .map(_._1)

  def publish(f: Long => Event): F[Unit] =
    for {
      n <- refTables.modifyState(TableState.nextEventId)
      _ <- topic.publish1(f(n))
    } yield ()

}

object EventService {

  def apply[F[_]: Concurrent: Logger](r: Ref[F, Tables]): F[EventService[F]] =
    Topic(Event.initialize).map(t => new EventService[F](t, r))

}

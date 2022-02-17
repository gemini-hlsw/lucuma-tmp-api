// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.effect.{Concurrent, Ref}
import cats.implicits._
import fs2.Stream
import fs2.concurrent.Topic
import lucuma.odb.api.model.{Database, Event}

/**
 *
 */
final class EventService[F[_]](
  topic:       Topic[F, Event],
  refDatabase: Ref[F, Database]
)(implicit F: Concurrent[F]) {

  def subscribe: Stream[F, Event] =
    topic.subscribe(100)

  def publish(f: Long => Event): F[Unit] =
    for {
      n <- refDatabase.updateAndGet(Database.lastEventId.modify(_+1)).map(_.lastEventId)
      _ <- topic.publish1(f(n))
    } yield ()

}

object EventService {

  def apply[F[_]: Concurrent](r: Ref[F, Database]): F[EventService[F]] =
    Topic[F, Event].map(t => new EventService[F](t, r))

}

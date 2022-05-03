// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.implicits._
import cats.effect.{Async, Ref}
import lucuma.odb.api.model.Database

/**
 * The main "repository" for the API server.  It is simply a collection of
 * repositories for the top-level types.
 */
trait OdbRepo[F[_]] {

  def database: Ref[F, Database]

  def dataset: DatasetRepo[F]

  def eventService: EventService[F]

  def executionEvent: ExecutionEventRepo[F]

  def observation: ObservationRepo[F]

  def program: ProgramRepo[F]

  def target: TargetRepo[F]

}

object OdbRepo {

  /**
   * Creates an empty ODB repository backed by a `Ref` containing `Tables`.
   */
  def create[F[_]: Async]: F[OdbRepo[F]] =
    fromDatabase[F](Database.empty)

  /**
   * Creates an ODB repository backed by a `Ref` containing the given `Tables`.
   */
  def fromDatabase[F[_]: Async](d: Database): F[OdbRepo[F]] =
    for {
      r <- Ref.of[F, Database](d)
      s <- EventService(r)
    } yield new OdbRepo[F] {

      override def database: Ref[F, Database] =
        r

      override def eventService: EventService[F] =
        s

      override def dataset: DatasetRepo[F] =
        DatasetRepo.create(r)

      override def executionEvent: ExecutionEventRepo[F] =
        ExecutionEventRepo.create(r)

      override def observation: ObservationRepo[F] =
        ObservationRepo.create(r, s)

      override def program: ProgramRepo[F] =
        ProgramRepo.create(r, s)

      override def target: TargetRepo[F] =
        TargetRepo.create[F](r, s)

    }



}

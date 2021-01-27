// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.implicits._
import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import io.chrisdavenport.log4cats.Logger

/**
 * The main "repository" for the API server.  It is simply a collection of
 * repositories for the top-level types.
 */
trait OdbRepo[F[_]] {

  def tables: Ref[F, Tables]

  def eventService: EventService[F]

  def asterism: AsterismRepo[F]

  def observation: ObservationRepo[F]

  def program: ProgramRepo[F]

  def target: TargetRepo[F]

}

object OdbRepo {

  /**
   * Creates an empty ODB repository backed by a `Ref` containing `Tables`.
   */
  def create[F[_]: Concurrent: Logger]: F[OdbRepo[F]] =
    fromTables[F](Tables.empty)

  /**
   * Creates an ODB repository backed by a `Ref` containing the given `Tables`.
   */
  def fromTables[F[_]: Concurrent: Logger](t: Tables): F[OdbRepo[F]] =
    for {
      r <- Ref.of[F, Tables](t)
      s <- EventService(r)
    } yield new OdbRepo[F] {

      private val ref: Ref[F, Tables] =
        DebuggingRef(r)

      override def tables: Ref[F, Tables] =
        ref

      override def eventService: EventService[F] =
        s

      override def asterism: AsterismRepo[F] =
        AsterismRepo.create(ref, s)

      override def observation: ObservationRepo[F] =
        ObservationRepo.create(ref, s)

      override def program: ProgramRepo[F] =
        ProgramRepo.create(ref, s)

      override def target: TargetRepo[F] =
        TargetRepo.create(ref, s)
    }



}
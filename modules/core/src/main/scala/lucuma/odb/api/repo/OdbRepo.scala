// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.implicits._
import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import org.typelevel.log4cats.Logger

/**
 * The main "repository" for the API server.  It is simply a collection of
 * repositories for the top-level types.
 */
trait OdbRepo[F[_]] {

  def tables: Ref[F, Tables]

  def eventService: EventService[F]

  def asterism: AsterismRepo[F]

  def constraintSet: ConstraintSetRepo[F]

  def executionEvent: ExecutionEventRepo[F]

  def observation: ObservationRepo[F]

  def program: ProgramRepo[F]

  def step: StepRepo[F]

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

      override def tables: Ref[F, Tables] =
        r

      override def eventService: EventService[F] =
        s

      override def asterism: AsterismRepo[F] =
        AsterismRepo.create(r, s)

      override def constraintSet: ConstraintSetRepo[F] =
        ConstraintSetRepo.create(r, s)

      override def executionEvent: ExecutionEventRepo[F] =
        ExecutionEventRepo.create(r)

      override def observation: ObservationRepo[F] =
        ObservationRepo.create(r, s)

      override def program: ProgramRepo[F] =
        ProgramRepo.create(r, s)

      override def step: StepRepo[F] =
        StepRepo.create[F](r)

      override def target: TargetRepo[F] =
        TargetRepo.create(r, s)

    }



}

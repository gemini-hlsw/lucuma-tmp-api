// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.data.EitherT
import cats.Order.catsKernelOrderingForOrder
import cats.effect.{Async, Ref}
import cats.syntax.applicative._
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import lucuma.core.model.{Observation, Program, Target}
import lucuma.core.util.Gid
import lucuma.odb.api.model.{Database, Event, InputError, Table}
import lucuma.odb.api.model.targetModel._
import lucuma.odb.api.model.targetModel.TargetModel.TargetEvent
import lucuma.odb.api.model.syntax.toplevel._
import lucuma.odb.api.model.syntax.databasestate._
import lucuma.odb.api.model.syntax.eitherinput._

import scala.collection.immutable.SortedSet

sealed trait TargetRepo[F[_]] extends TopLevelRepo[F, Target.Id, TargetModel] {

  /**
   * Selects the target with the given id.
   */
  def selectTarget(
    tid:            Target.Id,
    includeDeleted: Boolean = false
  ): F[Option[TargetModel]]

  /**
   * Selects the target with the given id, assuming it exists and throwing
   * otherwise.
   */
  def unsafeSelectTarget(
    tid:            Target.Id,
    includeDeleted: Boolean = false
  ): F[TargetModel]

  /**
   * Selects all targets associated with a program.
   */
  def selectProgramTargets(
    pid:            Program.Id,
    includeDeleted: Boolean = false
  ): F[SortedSet[TargetModel]]

  /**
   * Selects the asterism associated with the given observation.
   */
  def selectObservationAsterism(
    oid:            Observation.Id,
    includeDeleted: Boolean = false
  ): F[SortedSet[TargetModel]]

  def selectProgramAsterisms(
    pid:            Program.Id,
    includeDeleted: Boolean = false
  ): F[List[SortedSet[TargetModel]]]

  def selectObservationFirstTarget(
    oid:            Observation.Id,
    includeDeleted: Boolean = false
  ): F[Option[TargetModel]]

  def selectObservationTargetEnvironment(
    oid: Observation.Id
  ): F[Option[TargetEnvironmentModel]]

  def unsafeSelectObservationTargetEnvironment(
    oid: Observation.Id
  ): F[TargetEnvironmentModel]

  def insert(newTarget: TargetModel.CreateInput): F[TargetModel.CreateResult]

  def edit(edit: TargetModel.EditInput): F[TargetModel.EditResult]

  /**
   * Clones the target referenced by `existingTid`.  Uses the `suggestedTid` for
   * its ID if it is supplied by the caller and not currently in use.
   */
  def clone(
    cloneInput: TargetModel.CloneInput
  ): F[TargetModel.CloneResult]

}

object TargetRepo {

  def create[F[_]: Async](
    databaseRef:  Ref[F, Database],
    eventService: EventService[F]
  ): TargetRepo[F] =

    new TopLevelRepoBase[F, Target.Id, TargetModel](
      databaseRef,
      eventService,
      Database.lastTargetId,
      Database.targets.andThen(Table.rows),
      (editType, model) => TargetEvent(_, editType, model)
    ) with TargetRepo[F] {

      override def selectTarget(
        tid:            Target.Id,
        includeDeleted: Boolean
      ): F[Option[TargetModel]] =
        select(tid, includeDeleted)

      override def unsafeSelectTarget(
        tid:            Target.Id,
        includeDeleted: Boolean
      ): F[TargetModel] =
        unsafeSelect(tid, includeDeleted)

      override def selectProgramTargets(
        pid:            Program.Id,
        includeDeleted: Boolean
      ): F[SortedSet[TargetModel]] =
        databaseRef.get.map { db =>
          SortedSet.from {
            db.targets.rows.values.filter(t => t.programId === pid && (includeDeleted || t.isPresent))
          }
        }

      private def unsafeSelect[I: Gid, A](
        id: I
      )(
        f: I => F[Option[A]]
      ): F[A] =
        f(id).flatMap {
          case None    => ExecutionException.missingReference[F, I, A](id)
          case Some(a) => a.pure[F]
        }

      override def selectObservationAsterism(
        oid:            Observation.Id,
        includeDeleted: Boolean
      ): F[SortedSet[TargetModel]] =

        for {
          e   <- selectObservationTargetEnvironment(oid)
          tids = e.map(_.asterism).getOrElse(SortedSet.empty[Target.Id])
          ts  <- tids.toList.traverse(tid => unsafeSelectTarget(tid, includeDeleted))
        } yield SortedSet.from(ts)


      override def selectProgramAsterisms(
        pid:            Program.Id,
        includeDeleted: Boolean
      ): F[List[SortedSet[TargetModel]]] =
        databaseRef.get.map { tab =>
          tab
            .observations
            .rows
            .values
            .filter(o => o.programId === pid && (includeDeleted || o.isPresent))
            .map(_.targetEnvironment.asterism)
            .toList
            .distinct
            .map(_.map(tab.targets.rows.apply).filter(t => includeDeleted || t.isPresent))
        }

      override def selectObservationFirstTarget(
        oid:            Observation.Id,
        includeDeleted: Boolean
      ): F[Option[TargetModel]] =
        selectObservationAsterism(oid, includeDeleted).map(_.headOption)

      override def selectObservationTargetEnvironment(
        id: Observation.Id
      ): F[Option[TargetEnvironmentModel]] =
        databaseRef.get.map(_.observations.rows.get(id).map(_.targetEnvironment))

      override def unsafeSelectObservationTargetEnvironment(
        id: Observation.Id
      ): F[TargetEnvironmentModel] =
        unsafeSelect(id)(selectObservationTargetEnvironment)

      override def insert(
        newTarget: TargetModel.CreateInput
      ): F[TargetModel.CreateResult] = {

        val create: F[TargetModel] =
          EitherT(
            databaseRef.modify { db =>
              newTarget
                .createTarget
                .run(db)
                .fold(
                  err => (db, InputError.Exception(err).asLeft),
                  _.map(_.asRight)
                )
            }
          ).rethrowT

        for {
          t <- create
          _ <- eventService.publish(TargetEvent(_, Event.EditType.Created, t))
        } yield TargetModel.CreateResult(t)
      }

      override def edit(editInput: TargetModel.EditInput): F[TargetModel.EditResult] =
        for {
          ts <- databaseRef.modifyState(editInput.editor.flipF).flatMap(_.liftTo[F])
          _  <- ts.traverse(t => eventService.publish(TargetEvent.updated(t)))
        } yield TargetModel.EditResult(ts)

      override def clone(
        cloneInput: TargetModel.CloneInput
      ): F[TargetModel.CloneResult] =
        for {
          t <- databaseRef.modifyState(cloneInput.go.flipF).flatMap(_.liftTo[F])
          _ <- eventService.publish(TargetEvent.created(t.newTarget))
        } yield t


    }
}
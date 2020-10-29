// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.AsterismModel
import lucuma.odb.api.model.AsterismModel.{AsterismEvent, AsterismProgramLinks, Create}
import lucuma.odb.api.model.syntax.validatedinput._
import lucuma.core.model.{Asterism, Program, Target}
import cats._
import cats.data.State
import cats.effect.concurrent.Ref
import cats.syntax.apply._
import cats.syntax.eq._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._

sealed trait AsterismRepo[F[_]] extends TopLevelRepo[F, Asterism.Id, AsterismModel] {

  def selectAllForProgram(pid: Program.Id, includeDeleted: Boolean = false): F[List[AsterismModel]]

  def selectAllForTarget(tid: Target.Id, includeDeleted: Boolean = false): F[List[AsterismModel]]

  def insert[T <: AsterismModel](input: AsterismModel.Create[T]): F[T]

  def shareWithPrograms(input: AsterismProgramLinks): F[AsterismModel]

  def unshareWithPrograms(input: AsterismProgramLinks): F[AsterismModel]

}

object AsterismRepo {

  def create[F[_]: Monad](
    tablesRef:     Ref[F, Tables],
    eventService:  EventService[F]
  )(implicit M: MonadError[F, Throwable]): AsterismRepo[F] =

    new TopLevelRepoBase[F, Asterism.Id, AsterismModel](
      tablesRef,
      eventService,
      Tables.lastAsterismId,
      Tables.asterisms,
      AsterismEvent.apply
    ) with AsterismRepo[F]
      with LookupSupport {

      override def selectAllForProgram(pid: Program.Id, includeDeleted: Boolean): F[List[AsterismModel]] =
        tablesRef.get.map { t =>
          val ids = t.observations.values.filter(_.programId === pid).flatMap(_.asterismId.toList).toSet
          ids.foldLeft(List.empty[AsterismModel]) { (l, i) =>
            t.asterisms.get(i).fold(l)(_ :: l)
          }
        }.map(deletionFilter(includeDeleted))

      override def selectAllForTarget(tid: Target.Id, includeDeleted: Boolean): F[List[AsterismModel]] =
        tablesRef.get.map { t =>
          t.asterisms.values.filter(_.targetIds(tid)).toList
        }.map(deletionFilter(includeDeleted))

      private def addAsterism[T <: AsterismModel](
        asterismId: Option[Asterism.Id],
        programs:   Set[Program.Id],
        factory:    Asterism.Id => T
      ): State[Tables, T] =
        for {
          a   <- createAndInsert(asterismId, factory)
          _   <- TableState.shareAsterismWithPrograms(a, programs)
        } yield a

      override def insert[T <: AsterismModel](input: Create[T]): F[T] =
        constructAndPublish { t =>
          val existing = tryNotFindAsterism(t, input.asterismId)
          val targets  = input.targetIds.iterator.toList.traverse(tryFindTarget(t, _))
          val programs = input.programIds.traverse(tryFindProgram(t, _))
          val asterism = input.withId
          (existing, targets, programs, asterism).mapN((_, _, _, f) =>
            addAsterism(input.asterismId, input.programIds.toSet, f)
          )
        }

      private def programSharing(
        input: AsterismProgramLinks,
        f:     (AsterismModel, Set[Program.Id]) => State[Tables, Unit]
      ): F[AsterismModel] =
        tablesRef.modifyState {
          for {
            a  <- TableState.asterism(input.asterismId)
            ps <- input.programIds.traverse(TableState.program).map(_.sequence)
            r  <- (a, ps).traverseN { (am, _) => f(am, input.programIds.toSet).as(am) }
          } yield r
        }.flatMap(_.liftTo[F])

      override def shareWithPrograms(input: AsterismProgramLinks): F[AsterismModel] =
        programSharing(input, TableState.shareAsterismWithPrograms)

      override def unshareWithPrograms(input: AsterismProgramLinks): F[AsterismModel] =
        programSharing(input, TableState.unshareAsterismWithPrograms)

    }
}

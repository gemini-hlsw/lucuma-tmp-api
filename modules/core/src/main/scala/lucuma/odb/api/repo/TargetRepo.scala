// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{TargetModel, ValidatedInput}
import lucuma.odb.api.model.TargetModel.{CreateNonsidereal, CreateSidereal, TargetEvent, TargetProgramLinks}
import lucuma.odb.api.model.Existence._
import lucuma.odb.api.model.syntax.validatedinput._
import lucuma.core.model.{Program, Target}
import cats._
import cats.data.State
import cats.effect.concurrent.Ref
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._


sealed trait TargetRepo[F[_]] extends TopLevelRepo[F, Target.Id, TargetModel] {

  def selectAllForProgram(pid: Program.Id, includeDeleted: Boolean = false): F[List[TargetModel]]

  def insertNonsidereal(input: CreateNonsidereal): F[TargetModel]

  def insertSidereal(input: CreateSidereal): F[TargetModel]

  def shareWithPrograms(input: TargetProgramLinks): F[List[TargetModel]]

  def unshareWithPrograms(input: TargetProgramLinks): F[List[TargetModel]]

}

object TargetRepo {

  def create[F[_]: Monad](
    tablesRef:    Ref[F, Tables],
    eventService: EventService[F]
  )(implicit M: MonadError[F, Throwable]): TargetRepo[F] =

    new TopLevelRepoBase[F, Target.Id, TargetModel](
      tablesRef,
      eventService,
      Tables.lastTargetId,
      Tables.targets,
      TargetEvent.apply
    ) with TargetRepo[F]
      with LookupSupport {

      override def selectAllForProgram(
        pid:            Program.Id,
        includeDeleted: Boolean = false
      ): F[List[TargetModel]] =
        tablesRef.get.flatMap { tables =>
          tables.programTargets.selectRight(pid).toList.traverse { tid =>
            tryFindTarget(tables, tid).liftTo[F]
          }
        }.map(deletionFilter(includeDeleted))

      def addAndShare(id: Option[Target.Id], g: Target, pids: Set[Program.Id]): State[Tables, TargetModel] =
        for {
          t   <- createAndInsert(id, tid => TargetModel(tid, Present, g))
          _   <- TableState.shareTargetWithPrograms(t, pids)
        } yield t

      private def insertTarget(id: Option[Target.Id], pids: List[Program.Id], vt: ValidatedInput[Target]): F[TargetModel] =
        constructAndPublish { t =>
          (vt, tryNotFindTarget(t, id), pids.traverse(tryFindProgram(t, _))).mapN((g, _, _) =>
            addAndShare(id, g, pids.toSet)
          )
        }

      override def insertNonsidereal(input: CreateNonsidereal): F[TargetModel] =
        insertTarget(input.targetId, input.programIds.toList.flatten, input.toGemTarget)

      override def insertSidereal(input: CreateSidereal): F[TargetModel] =
        insertTarget(input.targetId, input.programIds.toList.flatten, input.toGemTarget)

      private def programSharing(
        input: TargetProgramLinks,
        f:     (TargetModel, Set[Program.Id]) => State[Tables, Unit]
      ): F[List[TargetModel]] =
        tablesRef.modifyState {
          for {
            ts <- input.targetIds.traverse(TableState.target).map(_.sequence)
            ps <- input.programIds.traverse(TableState.program).map(_.sequence)
            r  <- (ts, ps).traverseN { (tms, _) =>
              tms.traverse(tm => f(tm, input.programIds.toSet).as(tm))
            }
          } yield r
        }.flatMap(_.liftTo[F])

      override def shareWithPrograms(input: TargetProgramLinks): F[List[TargetModel]] =
        programSharing(input, TableState.shareTargetWithPrograms)

      override def unshareWithPrograms(input: TargetProgramLinks): F[List[TargetModel]] =
        programSharing(input, TableState.unshareTargetWithPrograms)

    }

}

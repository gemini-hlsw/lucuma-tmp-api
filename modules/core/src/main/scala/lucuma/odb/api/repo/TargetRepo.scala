// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{ProgramModel, TargetModel, ValidatedInput}
import lucuma.odb.api.model.TargetModel.{CreateNonsidereal, CreateSidereal, TargetEvent, TargetProgramLinks}
import lucuma.odb.api.model.Existence._
import lucuma.odb.api.model.syntax.validatedinput._
import lucuma.core.model.Target
import cats._
import cats.data.State
import cats.effect.concurrent.Ref
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._


sealed trait TargetRepo[F[_]] extends TopLevelRepo[F, TargetModel.Id, TargetModel] {

  def selectAllForProgram(pid: ProgramModel.Id, includeDeleted: Boolean = false): F[List[TargetModel]]

  def insertNonsidereal(input: CreateNonsidereal): F[TargetModel]

  def insertSidereal(input: CreateSidereal): F[TargetModel]

  def shareWithPrograms(input: TargetProgramLinks): F[TargetModel]

  def unshareWithPrograms(input: TargetProgramLinks): F[TargetModel]

}

object TargetRepo {

  def create[F[_]: Monad](
    tablesRef:    Ref[F, Tables],
    eventService: EventService[F]
  )(implicit M: MonadError[F, Throwable]): TargetRepo[F] =

    new TopLevelRepoBase[F, TargetModel.Id, TargetModel](
      tablesRef,
      eventService,
      Tables.lastTargetId,
      Tables.targets,
      TargetEvent.apply
    ) with TargetRepo[F]
      with LookupSupport {

      override def selectAllForProgram(
        pid:            ProgramModel.Id,
        includeDeleted: Boolean = false
      ): F[List[TargetModel]] =
        tablesRef.get.flatMap { tables =>
          tables.programTargets.selectRight(pid).toList.traverse { tid =>
            tables.targets.get(tid).fold(
              missingReference[F, TargetModel.Id, TargetModel](tid)
            )(M.pure)
          }
        }.map(deletionFilter(includeDeleted))

      def addAndShare(id: Option[TargetModel.Id], g: Target, pids: Set[ProgramModel.Id]): State[Tables, TargetModel] =
        for {
          t   <- createAndInsert(id, tid => TargetModel(tid, Present, g))
          _   <- Tables.shareTargetWithPrograms(t, pids)
        } yield t

      private def insertTarget(id: Option[TargetModel.Id], pids: List[ProgramModel.Id], vt: ValidatedInput[Target]): F[TargetModel] =
        constructAndPublish { t =>
          (vt, dontFindTarget(t, id), pids.traverse(lookupProgram(t, _))).mapN((g, _, _) =>
            addAndShare(id, g, pids.toSet)
          )
        }

      override def insertNonsidereal(input: CreateNonsidereal): F[TargetModel] =
        insertTarget(input.targetId, input.programIds, input.toGemTarget)

      override def insertSidereal(input: CreateSidereal): F[TargetModel] =
        insertTarget(input.targetId, input.programIds, input.toGemTarget)

      private def programSharing(
        input: TargetProgramLinks,
        f:     (TargetModel, Set[ProgramModel.Id]) => State[Tables, Unit]
      ): F[TargetModel] =
        tablesRef.modifyState {
          for {
            t  <- inspectTargetId(input.targetId)
            ps <- input.programIds.traverse(inspectProgramId).map(_.sequence)
            r  <- (t, ps).traverseN { (tm, _) => f(tm, input.programIds.toSet).as(tm) }
          } yield r
        }.flatMap(_.liftTo[F])

      override def shareWithPrograms(input: TargetProgramLinks): F[TargetModel] =
        programSharing(input, Tables.shareTargetWithPrograms)

      override def unshareWithPrograms(input: TargetProgramLinks): F[TargetModel] =
        programSharing(input, Tables.unshareTargetWithPrograms)

    }

}

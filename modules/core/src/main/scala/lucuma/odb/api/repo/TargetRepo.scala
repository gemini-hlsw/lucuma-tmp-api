// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{ProgramModel, TargetModel, ValidatedInput}
import lucuma.odb.api.model.TargetModel.{TargetCreatedEvent, TargetEditedEvent}
import lucuma.odb.api.model.Existence._
import cats._
import cats.data.State
import cats.implicits._
import cats.effect.concurrent.Ref


sealed trait TargetRepo[F[_]] extends TopLevelRepo[F, TargetModel.Id, TargetModel] {

  def selectAllForProgram(pid: ProgramModel.Id, includeDeleted: Boolean = false): F[List[TargetModel]]

  def insertNonsidereal(input: TargetModel.CreateNonsidereal): F[TargetModel]

  def insertSidereal(input: TargetModel.CreateSidereal): F[TargetModel]

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
      TargetCreatedEvent.apply,
      TargetEditedEvent.apply
    ) with TargetRepo[F]
      with LookupSupport[F] {

      override def selectAllForProgram(
                                        pid:            ProgramModel.Id,
                                        includeDeleted: Boolean = false
      ): F[List[TargetModel]] =
        tablesRef.get.flatMap { tables =>
          tables.programTargets.selectRight(pid).toList.traverse { tid =>
            tables.targets.get(tid).fold(missingTarget(tid))(M.pure)
          }
        }.map(deletionFilter(includeDeleted))

      def addAndShare(g: lucuma.core.model.Target, pids: Set[ProgramModel.Id]): State[Tables, TargetModel] =
        for {
          t   <- createAndInsert(tid => TargetModel(tid, Present, g))
          _   <- Tables.shareTarget(t, pids)
        } yield t

      private def insertTarget(pids: List[ProgramModel.Id], vt: ValidatedInput[lucuma.core.model.Target]): F[TargetModel] =
        modify { t =>
          // NOTE: look up all the supplied program ids to make sure they
          // correspond to real programs.  We ignore a successful result though.
          (vt, pids.traverse(lookupProgram(t, _)))
            .mapN((g, _) => addAndShare(g, pids.toSet).run(t).value)
            .fold(
              err => (t, err.asLeft[TargetModel]),
              tup => tup.map(_.asRight)
            )
        }

      override def insertNonsidereal(input: TargetModel.CreateNonsidereal): F[TargetModel] =
        insertTarget(input.pids, input.toGemTarget)

      override def insertSidereal(input: TargetModel.CreateSidereal): F[TargetModel] =
        insertTarget(input.pids, input.toGemTarget)

    }

}

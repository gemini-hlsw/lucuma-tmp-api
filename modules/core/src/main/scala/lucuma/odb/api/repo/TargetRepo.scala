// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{Program, Target, ValidatedInput}
import lucuma.odb.api.model.Target.{TargetCreatedEvent, TargetEditedEvent}
import lucuma.odb.api.model.Existence._
import cats._
import cats.data.State
import cats.implicits._
import cats.effect.concurrent.Ref


sealed trait TargetRepo[F[_]] extends TopLevelRepo[F, Target.Id, Target] {

  def selectAllForProgram(pid: Program.Id, includeDeleted: Boolean = false): F[List[Target]]

  def insertNonsidereal(input: Target.CreateNonsidereal): F[Target]

  def insertSidereal(input: Target.CreateSidereal): F[Target]

}

object TargetRepo {

  def create[F[_]: Monad](
    tablesRef:    Ref[F, Tables],
    eventService: EventService[F]
  )(implicit M: MonadError[F, Throwable]): TargetRepo[F] =

    new TopLevelRepoBase[F, Target.Id, Target](
      tablesRef,
      eventService,
      Tables.lastTargetId,
      Tables.targets,
      TargetCreatedEvent.apply,
      TargetEditedEvent.apply
    ) with TargetRepo[F]
      with LookupSupport[F] {

      override def selectAllForProgram(
        pid:            Program.Id,
        includeDeleted: Boolean = false
      ): F[List[Target]] =
        tablesRef.get.flatMap { tables =>
          tables.programTargets.selectRight(pid).toList.traverse { tid =>
            tables.targets.get(tid).fold(missingTarget(tid))(M.pure)
          }
        }.map(deletionFilter(includeDeleted))

      def addAndShare(g: lucuma.core.model.Target, pids: Set[Program.Id]): State[Tables, Target] =
        for {
          t   <- createAndInsert(tid => Target(tid, Present, g))
          _   <- Tables.shareTarget(t, pids)
        } yield t

      private def insertTarget(pids: List[Program.Id], vt: ValidatedInput[lucuma.core.model.Target]): F[Target] =
        modify { t =>
          // NOTE: look up all the supplied program ids to make sure they
          // correspond to real programs.  We ignore a successful result though.
          (vt, pids.traverse(lookupProgram(t, _)))
            .mapN((g, _) => addAndShare(g, pids.toSet).run(t).value)
            .fold(
              err => (t, err.asLeft[Target]),
              tup => tup.map(_.asRight)
            )
        }

      override def insertNonsidereal(input: Target.CreateNonsidereal): F[Target] =
        insertTarget(input.pids, input.toGemTarget)

      override def insertSidereal(input: Target.CreateSidereal): F[Target] =
        insertTarget(input.pids, input.toGemTarget.validNec)

    }

}

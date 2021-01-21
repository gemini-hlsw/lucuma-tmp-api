// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{ObservationModel, Sharing, TargetModel, ValidatedInput}
import lucuma.odb.api.model.TargetModel.{CreateNonsidereal, CreateSidereal, TargetEvent}
import lucuma.odb.api.model.Existence._
import lucuma.odb.api.model.syntax.validatedinput._
import lucuma.core.model.{Asterism, Observation, Program, Target}
import cats._
import cats.data.State
import cats.effect.concurrent.Ref
import cats.syntax.all._
import monocle.state.all._


sealed trait TargetRepo[F[_]] extends TopLevelRepo[F, Target.Id, TargetModel] {

  def selectAllForProgram(pid: Program.Id, includeDeleted: Boolean = false): F[List[TargetModel]]

  def selectAllForAsterism(aid: Asterism.Id, includeDeleted: Boolean = false): F[List[TargetModel]]

  def insertNonsidereal(input: CreateNonsidereal): F[TargetModel]

  def insertSidereal(input: CreateSidereal): F[TargetModel]

  def shareWithObservations(input: Sharing[Target.Id, Observation.Id]): F[TargetModel]

  def unshareWithObservations(input: Sharing[Target.Id, Observation.Id]): F[TargetModel]

  def shareWithPrograms(input: Sharing[Target.Id, Program.Id]): F[TargetModel]

  def unshareWithPrograms(input: Sharing[Target.Id, Program.Id]): F[TargetModel]

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
          tables.programTarget.selectRight(pid).toList.traverse { tid =>
            tryFindTarget(tables, tid).liftTo[F]
          }
        }.map(deletionFilter(includeDeleted))

      override def selectAllForAsterism(
        aid:            Asterism.Id,
        includeDeleted: Boolean = false
      ): F[List[TargetModel]] =
        tablesRef.get.flatMap { tables =>
          tables.targetAsterism.selectLeft(aid).toList.traverse { tid =>
            tryFindTarget(tables, tid).liftTo[F]
          }
        }.map(deletionFilter(includeDeleted))

      def addAndShare(id: Option[Target.Id], g: Target, pids: Set[Program.Id]): State[Tables, TargetModel] =
        for {
          t   <- createAndInsert(id, tid => TargetModel(tid, Present, g))
          _   <- Tables.programTarget.mod_(_ ++ pids.toList.tupleRight(t.id))
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

      def observationSharing(
        input:   Sharing[Target.Id, Observation.Id],
        targets: Option[Either[Asterism.Id, Target.Id]]
      ): F[TargetModel] =
        tablesRef.modifyState {
          for {
            t  <- TableState.target(input.one)
            os <- input.many.traverse(TableState.observation).map(_.sequence)
            r  <- (t, os).traverseN { (tm, oms) =>
              val updates = oms.map(om => (om.id, ObservationModel.targets.set(targets)(om)))
              Tables.observations.mod(_ ++ updates).as(tm)
            }
          } yield r
        }.flatMap(_.liftTo[F])

      override def shareWithObservations(input: Sharing[Target.Id, Observation.Id]): F[TargetModel] =
        observationSharing(input, input.one.asRight[Asterism.Id].some)

      override def unshareWithObservations(input: Sharing[Target.Id, Observation.Id]): F[TargetModel] =
        observationSharing(input, Option.empty)

      private def programSharing(
        input: Sharing[Target.Id, Program.Id],
        f:     ManyToMany[Program.Id, Target.Id] => ManyToMany[Program.Id, Target.Id]
      ): F[TargetModel] =
        tablesRef.modifyState {
          for {
            ts <- TableState.target(input.one)
            ps <- input.many.traverse(TableState.program).map(_.sequence)
            r  <- (ts, ps).traverseN { (tm, _) => Tables.programTarget.mod_(f).as(tm) }
          } yield r
        }.flatMap(_.liftTo[F])

      override def shareWithPrograms(input: Sharing[Target.Id, Program.Id]): F[TargetModel] =
        programSharing(input, _ ++ input.tupleRight)

      override def unshareWithPrograms(input: Sharing[Target.Id, Program.Id]): F[TargetModel] =
        programSharing(input, _ -- input.tupleRight)

    }

}

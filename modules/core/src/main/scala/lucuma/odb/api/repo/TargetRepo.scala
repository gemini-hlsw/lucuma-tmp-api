// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{AsterismModel, ObservationModel, ProgramModel, Sharing, TargetModel, ValidatedInput}
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

  def selectPageForProgram(
    pid:            Program.Id,
    count:          Int               = Integer.MAX_VALUE,
    afterGid:       Option[Target.Id] = None,
    includeDeleted: Boolean           = false
  ): F[ResultPage[TargetModel]]

  def selectPageForAsterism(
    aid:            Asterism.Id,
    count:          Int               = Integer.MAX_VALUE,
    afterGid:       Option[Target.Id] = None,
    includeDeleted: Boolean           = false
  ): F[ResultPage[TargetModel]]

  def insertNonsidereal(input: CreateNonsidereal): F[TargetModel]

  def insertSidereal(input: CreateSidereal): F[TargetModel]

  def shareWithAsterisms(input: Sharing[Target.Id, Asterism.Id]): F[TargetModel]

  def unshareWithAsterisms(input: Sharing[Target.Id, Asterism.Id]): F[TargetModel]

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
      (editType, model) => TargetEvent(_, editType, model)
    ) with TargetRepo[F]
      with LookupSupport {

      override def selectPageForProgram(
        pid:            Program.Id,
        count:          Int               = Integer.MAX_VALUE,
        afterGid:       Option[Target.Id] = None,
        includeDeleted: Boolean           = false
      ): F[ResultPage[TargetModel]] =

        selectPageFromIds(count, afterGid, includeDeleted) { tables =>
          tables.programTarget.selectRight(pid)
        }

      override def selectPageForAsterism(
        aid:            Asterism.Id,
        count:          Int               = Integer.MAX_VALUE,
        afterGid:       Option[Target.Id] = None,
        includeDeleted: Boolean           = false
      ): F[ResultPage[TargetModel]] =

        selectPageFromIds(count, afterGid, includeDeleted) { tables =>
          tables.targetAsterism.selectLeft(aid)
        }

      def addAndShare(id: Option[Target.Id], g: Target, pids: Set[Program.Id]): State[Tables, TargetModel] =
        for {
          t <- createAndInsert(id, tid => TargetModel(tid, Present, g))
          _ <- Tables.programTarget.mod_(_ ++ pids.toList.tupleRight(t.id))
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

      private def asterismSharing(
        input: Sharing[Target.Id, Asterism.Id]
      )(
        update: (ManyToMany[Target.Id, Asterism.Id], IterableOnce[(Target.Id, Asterism.Id)]) => ManyToMany[Target.Id, Asterism.Id]
      ): F[TargetModel] =
        shareRight[Asterism.Id, AsterismModel](
          "target", input, TableState.asterism, Tables.targetAsterism, AsterismModel.AsterismEvent.updated
        )(update)

      override def shareWithAsterisms(input: Sharing[Target.Id, Asterism.Id]): F[TargetModel] =
        asterismSharing(input)(_ ++ _)

      override def unshareWithAsterisms(input: Sharing[Target.Id, Asterism.Id]): F[TargetModel] =
        asterismSharing(input)(_ -- _)

      def observationSharing(
        input:   Sharing[Target.Id, Observation.Id],
        targets: Option[Either[Asterism.Id, Target.Id]]
      ): F[TargetModel] = {
        val link = tablesRef.modifyState {
          for {
            t  <- TableState.target(input.one)
            os <- input.many.traverse(TableState.observation).map(_.sequence)
            r  <- (t, os).traverseN { (tm, oms) =>
              val updates = oms.map(om => (om.id, ObservationModel.targets.set(targets)(om)))
              Tables.observations.mod(_ ++ updates).as((tm, oms))
            }
          } yield r
        }.flatMap(_.liftTo[F])

        for {
          tos    <- link
          (t, os) = tos
          _      <- eventService.publish(TargetModel.TargetEvent.updated(t))
          _      <- os.traverse_(o => eventService.publish(ObservationModel.ObservationEvent.updated(o)))
        } yield t
      }

      override def shareWithObservations(input: Sharing[Target.Id, Observation.Id]): F[TargetModel] =
        observationSharing(input, input.one.asRight[Asterism.Id].some)

      override def unshareWithObservations(input: Sharing[Target.Id, Observation.Id]): F[TargetModel] =
        observationSharing(input, Option.empty)

      private def programSharing(
        input: Sharing[Target.Id, Program.Id],
      )(
        update: (ManyToMany[Program.Id, Target.Id], IterableOnce[(Program.Id, Target.Id)]) => ManyToMany[Program.Id, Target.Id]
      ): F[TargetModel] =
        shareLeft[Program.Id, ProgramModel](
          "target", input, TableState.program, Tables.programTarget, ProgramModel.ProgramEvent.updated
        )(update)

      override def shareWithPrograms(input: Sharing[Target.Id, Program.Id]): F[TargetModel] =
        programSharing(input)(_ ++ _)

      override def unshareWithPrograms(input: Sharing[Target.Id, Program.Id]): F[TargetModel] =
        programSharing(input)(_ -- _)

    }

}

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{AsterismModel, ObservationModel, ProgramModel, Sharing, TargetModel}
import lucuma.odb.api.model.AsterismModel.{AsterismEvent, Create}
import lucuma.odb.api.model.syntax.validatedinput._
import lucuma.core.model.{Asterism, Observation, Program, Target}
import cats._
import cats.data.State
import cats.effect.concurrent.Ref
import cats.syntax.all._
import monocle.state.all._

import scala.collection.immutable.SortedSet

sealed trait AsterismRepo[F[_]] extends TopLevelRepo[F, Asterism.Id, AsterismModel] {

  def selectPageForProgram(
    pid:            Program.Id,
    count:          Int                 = Integer.MAX_VALUE,
    afterGid:       Option[Asterism.Id] = None,
    includeDeleted: Boolean             = false
  ): F[ResultPage[AsterismModel]]

  def selectPageForTarget(
    tid:            Target.Id,
    pid:            Option[Program.Id],
    count:          Int                 = Integer.MAX_VALUE,
    afterGid:       Option[Asterism.Id] = None,
    includeDeleted: Boolean             = false
  ): F[ResultPage[AsterismModel]]

  def selectForObservation(oid: Observation.Id, includeDeleted: Boolean = false): F[Option[AsterismModel]]

  def insert(input: AsterismModel.Create): F[AsterismModel]

  def shareWithObservations(input: Sharing[Asterism.Id, Observation.Id]): F[AsterismModel]

  def unshareWithObservations(input: Sharing[Asterism.Id, Observation.Id]): F[AsterismModel]

  def shareWithPrograms(input: Sharing[Asterism.Id, Program.Id]): F[AsterismModel]

  def unshareWithPrograms(input: Sharing[Asterism.Id, Program.Id]): F[AsterismModel]

  def shareWithTargets(input: Sharing[Asterism.Id, Target.Id]): F[AsterismModel]

  def unshareWithTargets(input: Sharing[Asterism.Id, Target.Id]): F[AsterismModel]

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
      (editType, model) => AsterismEvent(_, editType, model)
    ) with AsterismRepo[F]
      with LookupSupport {

      private def asterismIdsForProgram(
        tables: Tables,
        pid:    Program.Id
      ): SortedSet[Asterism.Id] = {
        val fromObs = tables
                       .observations
                       .values
                       .filter(_.programId === pid)
                       .map(_.targets)
                       .collect { case Some(Left(aid)) => aid }
                       .toSet

        val fromProg = tables.programAsterism.selectRight(pid)

        fromProg ++ fromObs
      }


      override def selectPageForProgram(
        pid:            Program.Id,
        count:          Int,
        afterGid:       Option[Asterism.Id],
        includeDeleted: Boolean
      ): F[ResultPage[AsterismModel]] =
        selectPageFromIds(count, afterGid, includeDeleted)(asterismIdsForProgram(_, pid))

      override def selectPageForTarget(
        tid:            Target.Id,
        pid:            Option[Program.Id], // limit to this program
        count:          Int,
        afterGid:       Option[Asterism.Id],
        includeDeleted: Boolean
      ): F[ResultPage[AsterismModel]] =

        selectPageFromIds(count, afterGid, includeDeleted) { tables =>
          val ids = tables.targetAsterism.selectRight(tid)
          pid.fold(ids) { p => asterismIdsForProgram(tables, p).intersect(ids) }
        }

      override def selectForObservation(oid: Observation.Id, includeDeleted: Boolean): F[Option[AsterismModel]] =
        tablesRef.get.map { t =>
          t.observations.get(oid).flatMap(_.targets).collect {
            case Left(aid) => aid
          }.flatMap(t.asterisms.get)
        }

      private def addAsterism[T <: AsterismModel](
        asterismId: Option[Asterism.Id],
        programs:   Set[Program.Id],
        factory:    Asterism.Id => T
      ): State[Tables, T] =
        for {
          a   <- createAndInsert(asterismId, factory)
          _   <- Tables.programAsterism.mod_(_ ++ programs.toList.tupleRight(a.id))
        } yield a

      override def insert(input: Create): F[AsterismModel] =
        constructAndPublish { t =>
          val existing = tryNotFindAsterism(t, input.asterismId)
          val programs = input.programIds.traverse(tryFindProgram(t, _))
          val asterism = input.withId
          (existing, programs, asterism).mapN((_, _, f) =>
            addAsterism(input.asterismId, input.programIds.toSet, f)
          )
        }

      def obsSharing(
        input:   Sharing[Asterism.Id, Observation.Id],
        targets: Option[Either[Asterism.Id, Target.Id]]
      ): F[AsterismModel] = {
        val link = tablesRef.modifyState {
          for {
            a  <- TableState.asterism(input.one)
            os <- input.many.traverse(TableState.observation).map(_.sequence)
            r  <- (a, os).traverseN { (am, oms) =>
              val updates = oms.map(om => (om.id, ObservationModel.targets.set(targets)(om)))
              Tables.observations.mod(_ ++ updates).as((am, oms))
            }
          } yield r
        }.flatMap(_.liftTo[F])

        for {
          aos    <- link
          (a, os) = aos
          _      <- eventService.publish(AsterismModel.AsterismEvent.updated(a))
          _      <- os.traverse_(o => eventService.publish(ObservationModel.ObservationEvent.updated(o)))
        } yield a
      }

      override def shareWithObservations(input: Sharing[Asterism.Id, Observation.Id]): F[AsterismModel] =
        obsSharing(input, input.one.asLeft[Target.Id].some)

      override def unshareWithObservations(input: Sharing[Asterism.Id, Observation.Id]): F[AsterismModel] =
        obsSharing(input, Option.empty)

      def programSharing(
        input: Sharing[Asterism.Id, Program.Id]
      )(
        update: (ManyToMany[Program.Id, Asterism.Id], IterableOnce[(Program.Id, Asterism.Id)]) => ManyToMany[Program.Id, Asterism.Id]
      ): F[AsterismModel] =
        shareLeft[Program.Id, ProgramModel](
          "asterism",
          input,
          TableState.program,
          Tables.programAsterism,
          ProgramModel.ProgramEvent.updated
        )(update)

      override def shareWithPrograms(input: Sharing[Asterism.Id, Program.Id]): F[AsterismModel] =
        programSharing(input)(_ ++ _)

      override def unshareWithPrograms(input: Sharing[Asterism.Id, Program.Id]): F[AsterismModel] =
        programSharing(input)(_ -- _)

      def targetSharing(
        input: Sharing[Asterism.Id, Target.Id]
      )(
        update: (ManyToMany[Target.Id, Asterism.Id], IterableOnce[(Target.Id, Asterism.Id)]) => ManyToMany[Target.Id, Asterism.Id]
      ): F[AsterismModel] =
        shareLeft[Target.Id, TargetModel](
          "asterism", input, TableState.target, Tables.targetAsterism, TargetModel.TargetEvent.updated
        )(update)

      override def shareWithTargets(input: Sharing[Asterism.Id, Target.Id]): F[AsterismModel] =
        targetSharing(input)(_ ++ _)

      override def unshareWithTargets(input: Sharing[Asterism.Id, Target.Id]): F[AsterismModel] =
        targetSharing(input)(_ -- _)

    }
}

// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{AsterismModel, Event, InputError, ProgramModel, Sharing, TargetModel}
import lucuma.odb.api.model.AsterismModel.{AsterismEvent, Create}
import lucuma.odb.api.model.syntax.toplevel._
import lucuma.core.model.{Asterism, Observation, Program, Target}
import cats._
import cats.data.{EitherT, State}
import cats.effect.Ref
import cats.mtl.Stateful._
import cats.syntax.all._

import scala.collection.immutable.SortedSet

sealed trait AsterismRepo[F[_]] extends TopLevelRepo[F, Asterism.Id, AsterismModel] {

  def selectPageForProgram(
    pid:            Program.Id,
    count:          Option[Int]         = None,
    afterGid:       Option[Asterism.Id] = None,
    includeDeleted: Boolean             = false
  ): F[ResultPage[AsterismModel]]

  def selectPageForTarget(
    tid:            Target.Id,
    pid:            Option[Program.Id]  = None,
    count:          Option[Int]         = None,
    afterGid:       Option[Asterism.Id] = None,
    includeDeleted: Boolean             = false
  ): F[ResultPage[AsterismModel]]

  def selectForObservation(oid: Observation.Id, includeDeleted: Boolean = false): F[Option[AsterismModel]]

  def insert(input: AsterismModel.Create): F[AsterismModel]

  def shareWithPrograms(input: Sharing[Asterism.Id, Program.Id]): F[AsterismModel]

  def unshareWithPrograms(input: Sharing[Asterism.Id, Program.Id]): F[AsterismModel]

  def shareWithTargets(input: Sharing[Asterism.Id, Target.Id]): F[AsterismModel]

  def unshareWithTargets(input: Sharing[Asterism.Id, Target.Id]): F[AsterismModel]

}

object AsterismRepo {

  def create[F[_]](
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
                       .map(_.pointing)
                       .collect { case Some(Left(aid)) => aid }

        val fromProg = tables.programAsterism.selectRight(pid)

        fromProg ++ fromObs
      }

      override def selectPageForProgram(
        pid:            Program.Id,
        count:          Option[Int],
        afterGid:       Option[Asterism.Id],
        includeDeleted: Boolean
      ): F[ResultPage[AsterismModel]] =

        selectPageFromIds(count, afterGid, includeDeleted)(asterismIdsForProgram(_, pid))

      override def selectPageForTarget(
        tid:            Target.Id,
        pid:            Option[Program.Id],
        count:          Option[Int],
        afterGid:       Option[Asterism.Id],
        includeDeleted: Boolean
      ): F[ResultPage[AsterismModel]] =

        selectPageFromIds(count, afterGid, includeDeleted) { tables =>
          val ids = tables.targetAsterism.selectRight(tid)
          pid.fold(ids) { p => asterismIdsForProgram(tables, p).intersect(ids) }
        }

      override def selectForObservation(oid: Observation.Id, includeDeleted: Boolean): F[Option[AsterismModel]] =
        tablesRef.get.map { t =>
          t.observations.get(oid).flatMap(_.pointing).collect {
            case Left(aid) => aid
          }
          .flatMap(t.asterisms.get)
          .filter(a => includeDeleted || a.isPresent)
        }

      override def insert(input: Create): F[AsterismModel] = {
        val create = EitherT(
          tablesRef.modify { tables =>

            val (tablesʹ, a) =
              input
                .create[State[Tables, *], Tables](TableState)
                .run(tables)
                .value

            a.fold(
              err => (tables,  InputError.Exception(err).asLeft),
              am  => (tablesʹ, am.asRight)
             )
          }
        ).rethrowT

        for {
          a <- create
          _ <- eventService.publish(AsterismEvent(_, Event.EditType.Created, a))
        } yield a

      }

      def programSharing(
        input: Sharing[Asterism.Id, Program.Id]
      )(
        update: (ManyToMany[Program.Id, Asterism.Id], IterableOnce[(Program.Id, Asterism.Id)]) => ManyToMany[Program.Id, Asterism.Id]
      ): F[AsterismModel] =
        shareLeft[Program.Id, ProgramModel](
          "asterism",
          input,
          TableState.program.lookupValidated[State[Tables, *]],
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
          "asterism", input, TableState.target.lookupValidated[State[Tables, *]], Tables.targetAsterism, TargetModel.TargetEvent.updated
        )(update)

      override def shareWithTargets(input: Sharing[Asterism.Id, Target.Id]): F[AsterismModel] =
        targetSharing(input)(_ ++ _)

      override def unshareWithTargets(input: Sharing[Asterism.Id, Target.Id]): F[AsterismModel] =
        targetSharing(input)(_ -- _)

    }
}

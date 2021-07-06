// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{AsterismModel, Event, InputError, ProgramModel, Sharing, TargetModel, ValidatedInput}
import lucuma.odb.api.model.TargetModel.{CreateNonsidereal, CreateSidereal, TargetEvent}
import lucuma.core.model.{Asterism, Program, Target}
import cats._
import cats.data.{EitherT, State}
import cats.effect.Ref
import cats.syntax.all._
import monocle.state.all._


sealed trait TargetRepo[F[_]] extends TopLevelRepo[F, Target.Id, TargetModel] {

  def selectPageForProgram(
    pid:            Program.Id,
    count:          Option[Int]       = None,
    afterGid:       Option[Target.Id] = None,
    includeDeleted: Boolean           = false
  ): F[ResultPage[TargetModel]]

  def selectPageForAsterism(
    aid:            Asterism.Id,
    count:          Option[Int]       = None,
    afterGid:       Option[Target.Id] = None,
    includeDeleted: Boolean           = false
  ): F[ResultPage[TargetModel]]

  def insertNonsidereal(input: CreateNonsidereal): F[TargetModel]

  def insertSidereal(input: CreateSidereal): F[TargetModel]

  def shareWithAsterisms(input: Sharing[Target.Id, Asterism.Id]): F[TargetModel]

  def unshareWithAsterisms(input: Sharing[Target.Id, Asterism.Id]): F[TargetModel]

  def shareWithPrograms(input: Sharing[Target.Id, Program.Id]): F[TargetModel]

  def unshareWithPrograms(input: Sharing[Target.Id, Program.Id]): F[TargetModel]

}

object TargetRepo {

  def create[F[_]](
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
        count:          Option[Int]       = None,
        afterGid:       Option[Target.Id] = None,
        includeDeleted: Boolean           = false
      ): F[ResultPage[TargetModel]] =

        selectPageFromIds(count, afterGid, includeDeleted) { tables =>
          tables.programTarget.selectRight(pid) ++
            tables.observations.values.filter(_.programId === pid).map(_.pointing).collect {
              case Some(Right(tid)) => tid
            }
        }

      override def selectPageForAsterism(
        aid:            Asterism.Id,
        count:          Option[Int]       = None,
        afterGid:       Option[Target.Id] = None,
        includeDeleted: Boolean           = false
      ): F[ResultPage[TargetModel]] =

        selectPageFromIds(count, afterGid, includeDeleted) { tables =>
          tables.targetAsterism.selectLeft(aid)
        }

      private def insertTarget(
        pids: Option[List[Program.Id]],
        tm:   State[Tables, ValidatedInput[TargetModel]]
      ): F[TargetModel] = {
        val create = EitherT(
          tablesRef.modify { tables =>
            val (tablesʹ, t) = (for {
              t <- tm
              _ <- t.traverse(tm => Tables.programTarget.mod_(_ ++ pids.toList.flatten.tupleRight(tm.id)))
            } yield t).run(tables).value

            t.fold(
              err => (tables,  InputError.Exception(err).asLeft),
              tm  => (tablesʹ, tm.asRight)
            )
          }
        ).rethrowT

        for {
          t <- create
          _ <- eventService.publish(TargetEvent(_, Event.EditType.Created, t))
        } yield t
      }


      override def insertNonsidereal(input: CreateNonsidereal): F[TargetModel] =
        insertTarget(input.programIds, input.create[State[Tables, *], Tables](TableState))

      override def insertSidereal(input: CreateSidereal): F[TargetModel] =
        insertTarget(input.programIds, input.create[State[Tables, *], Tables](TableState))

      private def asterismSharing(
        input: Sharing[Target.Id, Asterism.Id]
      )(
        update: (ManyToMany[Target.Id, Asterism.Id], IterableOnce[(Target.Id, Asterism.Id)]) => ManyToMany[Target.Id, Asterism.Id]
      ): F[TargetModel] =
        shareRight[Asterism.Id, AsterismModel](
          "target", input, TableState.asterism.lookupValidated[State[Tables, *]], Tables.targetAsterism, AsterismModel.AsterismEvent.updated
        )(update)

      override def shareWithAsterisms(input: Sharing[Target.Id, Asterism.Id]): F[TargetModel] =
        asterismSharing(input)(_ ++ _)

      override def unshareWithAsterisms(input: Sharing[Target.Id, Asterism.Id]): F[TargetModel] =
        asterismSharing(input)(_ -- _)

      private def programSharing(
        input: Sharing[Target.Id, Program.Id],
      )(
        update: (ManyToMany[Program.Id, Target.Id], IterableOnce[(Program.Id, Target.Id)]) => ManyToMany[Program.Id, Target.Id]
      ): F[TargetModel] =
        shareLeft[Program.Id, ProgramModel](
          "target", input, TableState.program.lookupValidated[State[Tables, *]], Tables.programTarget, ProgramModel.ProgramEvent.updated
        )(update)

      override def shareWithPrograms(input: Sharing[Target.Id, Program.Id]): F[TargetModel] =
        programSharing(input)(_ ++ _)

      override def unshareWithPrograms(input: Sharing[Target.Id, Program.Id]): F[TargetModel] =
        programSharing(input)(_ -- _)

    }

}

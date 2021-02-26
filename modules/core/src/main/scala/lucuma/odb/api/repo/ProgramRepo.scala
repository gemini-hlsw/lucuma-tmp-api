// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.ProgramModel
import lucuma.odb.api.model.ProgramModel.ProgramEvent
import lucuma.odb.api.model.Existence._
import lucuma.core.model.{Asterism, Program, Target}
import cats.Monad
import cats.implicits._
import cats.MonadError
import cats.effect.concurrent.Ref


trait ProgramRepo[F[_]] extends TopLevelRepo[F, Program.Id, ProgramModel] {

  def selectPageForAsterism(
    aid:            Asterism.Id,
    count:          Int                = Integer.MAX_VALUE,
    afterGid:       Option[Program.Id] = None,
    includeDeleted: Boolean            = false
  ): F[ResultPage[ProgramModel]]

  def selectPageForTarget(
    tid:            Target.Id,
    count:          Int                = Integer.MAX_VALUE,
    afterGid:       Option[Program.Id] = None,
    includeDeleted: Boolean            = false
  ): F[ResultPage[ProgramModel]]

  def insert(input: ProgramModel.Create): F[ProgramModel]

}

object ProgramRepo {

  def create[F[_]: Monad](
    tablesRef:    Ref[F, Tables],
    eventService: EventService[F]
  )(implicit M: MonadError[F, Throwable]): ProgramRepo[F] =

    new TopLevelRepoBase[F, Program.Id, ProgramModel](
      tablesRef,
      eventService,
      Tables.lastProgramId,
      Tables.programs,
      (editType, model) => ProgramEvent(_, editType, model)
    ) with ProgramRepo[F]
      with LookupSupport {

      private def havingObservationsOf(
        tables:  Tables,
        targets: Either[Asterism.Id, Target.Id]
      ): Iterable[Program.Id] =
        tables.observations.values.filter(_.targets.contains(targets)).map(_.programId)

      override def selectPageForAsterism(
        aid:            Asterism.Id,
        count:          Int,
        afterGid:       Option[Program.Id],
        includeDeleted: Boolean
      ): F[ResultPage[ProgramModel]] =

        selectPageFromIds(count, afterGid, includeDeleted) { tables =>
          tables.programAsterism.selectLeft(aid) ++
            havingObservationsOf(tables, aid.asLeft)
        }

      override def selectPageForTarget(
        tid:            Target.Id,
        count:          Int                = Integer.MAX_VALUE,
        afterGid:       Option[Program.Id] = None,
        includeDeleted: Boolean            = false
      ): F[ResultPage[ProgramModel]] =

        selectPageFromIds(count, afterGid, includeDeleted) { tables =>
          tables.programTarget.selectLeft(tid) ++
            havingObservationsOf(tables, tid.asRight)
        }

      override def insert(input: ProgramModel.Create): F[ProgramModel] =
        constructAndPublish { t =>
          tryNotFindProgram(t, input.programId).as(
            createAndInsert(input.programId, ProgramModel(_, Present, input.name))
          )
        }

    }

}
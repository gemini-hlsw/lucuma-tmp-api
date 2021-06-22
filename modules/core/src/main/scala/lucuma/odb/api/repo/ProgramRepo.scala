// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{Event, InputError, ProgramModel}
import lucuma.odb.api.model.ProgramModel.ProgramEvent
import lucuma.core.model.{Asterism, Program, Target}
import cats.Monad
import cats.implicits._
import cats.MonadError
import cats.data.{EitherT, State}
import cats.effect.Ref


trait ProgramRepo[F[_]] extends TopLevelRepo[F, Program.Id, ProgramModel] {

  /**
   * Selects a page of results corresponding to the given asterism id.
   *
   * @param aid the asterism associated with all the resulting programs in the
   *            page
   * @param includeObservations whether to include programs only linked through
   *                            observations that use the asterism
   * @param count how many results to ask for on the page
   * @param afterGid start the result set after the given program, if any
   * @param includeDeleted whether to include deleted results
   *
   * @return the result page, including a flag indicating whether more results
   *         remain
   */
  def selectPageForAsterism(
    aid:                 Asterism.Id,
    includeObservations: Boolean            = true,
    count:               Option[Int]        = None,
    afterGid:            Option[Program.Id] = None,
    includeDeleted:      Boolean            = false
  ): F[ResultPage[ProgramModel]]

  def selectPageForPrograms(
    pids:           Set[Program.Id],
    count:          Option[Int]        = None,
    afterGid:       Option[Program.Id] = None,
    includeDeleted: Boolean            = false
  ): F[ResultPage[ProgramModel]]

  /**
   * Selects a page of results corresponding to the given target id.
   *
   * @param tid the target associated with all the resulting programs in the
   *            page
   * @param includeObservations whether to include programs only linked through
   *                            observations that use the target
   * @param count how many results to ask for on the page
   * @param afterGid start the result set after the given program, if any
   * @param includeDeleted whether to include deleted results
   *
   * @return the result page, including a flag indicating whether more results
   *         remain
   */
  def selectPageForTarget(
    tid:                 Target.Id,
    includeObservations: Boolean            = true,
    count:               Option[Int]        = None,
    afterGid:            Option[Program.Id] = None,
    includeDeleted:      Boolean            = false
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
        tables.observations.values.filter(_.pointing.contains(targets)).map(_.programId)

      override def selectPageForAsterism(
        aid:                 Asterism.Id,
        includeObservations: Boolean,
        count:               Option[Int],
        afterGid:            Option[Program.Id],
        includeDeleted:      Boolean
      ): F[ResultPage[ProgramModel]] =

        selectPageFromIds(count, afterGid, includeDeleted) { tables =>
          tables.programAsterism.selectLeft(aid) ++
            (if (includeObservations) havingObservationsOf(tables, aid.asLeft)
            else Iterable.empty[Program.Id])
        }

      override def selectPageForPrograms(
        pids:           Set[Program.Id],
        count:          Option[Int]        = None,
        afterGid:       Option[Program.Id] = None,
        includeDeleted: Boolean            = false
      ): F[ResultPage[ProgramModel]] =

        selectPageFiltered(count, afterGid, includeDeleted) { p => pids(p.id) }

      override def selectPageForTarget(
        tid:                 Target.Id,
        includeObservations: Boolean,
        count:               Option[Int],
        afterGid:            Option[Program.Id],
        includeDeleted:      Boolean
      ): F[ResultPage[ProgramModel]] =

        selectPageFromIds(count, afterGid, includeDeleted) { tables =>
          tables.programTarget.selectLeft(tid) ++
            (if (includeObservations) havingObservationsOf(tables, tid.asRight)
            else Iterable.empty[Program.Id])
        }

      override def insert(input: ProgramModel.Create): F[ProgramModel] = {
        val create = EitherT(
          tablesRef.modify { tables =>
            val (tablesʹ, p) = input.create[State[Tables, *], Tables](TableState).run(tables).value

            p.fold(
              err => (tables,  InputError.Exception(err).asLeft),
              pm  => (tablesʹ, pm.asRight)
            )
          }
        ).rethrowT

        for {
          p <- create
          _ <- eventService.publish(ProgramEvent(_, Event.EditType.Created, p))
        } yield p
      }

    }

}
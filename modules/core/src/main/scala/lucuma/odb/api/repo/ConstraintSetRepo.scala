// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats._
import cats.data.EitherT
import cats.effect.concurrent.Ref
import cats.syntax.all._
import lucuma.core.model.{ConstraintSet, Observation, Program}
import lucuma.odb.api.model.{ConstraintSetModel, Event, InputError}
import lucuma.odb.api.model.ConstraintSetModel.ConstraintSetEvent
import lucuma.odb.api.model.syntax.toplevel._

trait ConstraintSetRepo[F[_]] extends TopLevelRepo[F, ConstraintSet.Id, ConstraintSetModel] {

  def selectForObservation(
    oid:            Observation.Id,
    includeDeleted: Boolean = false
  ): F[Option[ConstraintSetModel]]

  def selectPageForProgram(
    pid:            Program.Id,
    count:          Int                      = Integer.MAX_VALUE,
    afterGid:       Option[ConstraintSet.Id] = None,
    includeDeleted: Boolean                  = false
  ): F[ResultPage[ConstraintSetModel]]

  def insert(input: ConstraintSetModel.Create): F[ConstraintSetModel]
}

object ConstraintSetRepo {

  def create[F[_]: Monad](
    tablesRef:    Ref[F, Tables],
    eventService: EventService[F]
  )(implicit M:   MonadError[F, Throwable]
  ): ConstraintSetRepo[F] =
    new TopLevelRepoBase[F, ConstraintSet.Id, ConstraintSetModel](
      tablesRef,
      eventService,
      Tables.lastConstraintSetId,
      Tables.constraintSets,
      (editType, model) => ConstraintSetEvent(_, editType, model)
    ) with ConstraintSetRepo[F] with LookupSupport {

      override def selectForObservation(
        oid:            Observation.Id,
        includeDeleted: Boolean
      ): F[Option[ConstraintSetModel]] =
        tablesRef.get.map{ t =>
          for {
            o <- t.observations.get(oid)
            csid <- o.constraintSetId
            cs <- t.constraintSets.get(csid)
            if includeDeleted || cs.isPresent
          } yield cs
        }

      override def selectPageForProgram(
        pid:            Program.Id,
        count:          Int,
        afterGid:       Option[ConstraintSet.Id],
        includeDeleted: Boolean
      ): F[ResultPage[ConstraintSetModel]] =

        selectPageFiltered(count, afterGid, includeDeleted) { _.programId === pid }

      override def insert(input: ConstraintSetModel.Create): F[ConstraintSetModel] = {
        val create = EitherT(
          tablesRef.modify { tables =>
            val (tablesʹ, c) = input.create(TableState).run(tables).value

            c.fold(
              err => (tables,  InputError.Exception(err).asLeft),
              cm  => (tablesʹ, cm.asRight)
            )
          }
        ).rethrowT

        for {
          c <- create
          _ <- eventService.publish(ConstraintSetEvent(_, Event.EditType.Created, c))
        } yield c
      }
    }
}

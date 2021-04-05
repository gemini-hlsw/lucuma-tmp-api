// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats._
import cats.syntax.all._
import lucuma.core.model.{ConstraintSet, Observation, Program}
import lucuma.odb.api.model.ConstraintSetModel
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

      override def insert(newCs: ConstraintSetModel.Create): F[ConstraintSetModel] =
        constructAndPublish { t =>
          (tryNotFindConstraintSet(t, newCs.constraintSetId) *>
            tryFindProgram(t, newCs.programId) *>
            newCs.withId).map(createAndInsert(newCs.constraintSetId, _))
        }
    }
}

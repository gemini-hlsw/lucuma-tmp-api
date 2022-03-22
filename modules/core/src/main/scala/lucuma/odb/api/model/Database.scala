// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.Order.catsKernelOrderingForOrder
import cats.syntax.functor._
import lucuma.core.model.{ExecutionEvent, Observation, Program, Target}
import lucuma.odb.api.model.targetModel.TargetModel
import monocle.function.At
import monocle.{Focus, Lens, Optional, Prism}

import scala.collection.immutable.{ListMap, SortedMap}

final case class Database(
  lastEventId:     Long,
  visitRecords:    SortedMap[Observation.Id, VisitRecords],
  executionEvents: Table[ExecutionEvent.Id, ExecutionEventModel],
  observations:    Table[Observation.Id, ObservationModel],
  programs:        Table[Program.Id, ProgramModel],
  targets:         Table[Target.Id, TargetModel],
)

object Database extends DatabaseOptics {

  val empty: Database =
    Database(
      lastEventId     = 0L,
      visitRecords    = SortedMap.empty,
      executionEvents = Table.empty,
      observations    = Table.empty,
      programs        = Table.empty,
      targets         = Table.empty
    )

  implicit val EqDatabase: Eq[Database] =
    Eq.by { a => (
      a.lastEventId,
      a.visitRecords,
      a.executionEvents,
      a.observations,
      a.programs,
      a.targets
    )}

  val executionEvent: DatabaseState[ExecutionEvent.Id, ExecutionEventModel] =
    DatabaseState(executionEvents)

  val observation: DatabaseState[Observation.Id, ObservationModel] =
    DatabaseState(observations)

  val program: DatabaseState[Program.Id, ProgramModel] =
    DatabaseState(programs)

  val target: DatabaseState[Target.Id, TargetModel] =
    DatabaseState(targets)

}

sealed trait DatabaseOptics { self: Database.type =>

  val lastEventId: Lens[Database, Long] =
    Focus[Database](_.lastEventId)

  val visitRecords: Lens[Database, SortedMap[Observation.Id, VisitRecords]] =
    Focus[Database](_.visitRecords)

  def visitRecordsAt(oid: Observation.Id): Lens[Database, Option[VisitRecords]] =
    visitRecords.andThen(At.at(oid))

  def visitRecordsAtOptional(oid: Observation.Id): Optional[Database, VisitRecords] =
    visitRecords.andThen(
      Optional.apply[SortedMap[Observation.Id, VisitRecords], VisitRecords](_.get(oid)) { vr =>
        _.updatedWith(oid)(_.as(vr))
      }
    )

  def visitRecordAt[S, D](
    oid:     Observation.Id,
    visitId: Visit.Id,
    prism:   Prism[VisitRecords, ListMap[Visit.Id, VisitRecord[S, D]]]
  ): Optional[Database, VisitRecord[S, D]] =
    visitRecordsAtOptional(oid).andThen(prism.andThen(
      Optional.apply[ListMap[Visit.Id, VisitRecord[S, D]], VisitRecord[S, D]](_.get(visitId)) { vr =>
        _.updatedWith(visitId)(_.as(vr))
      }
    ))

  val executionEvents: Lens[Database, Table[ExecutionEvent.Id, ExecutionEventModel]] =
    Focus[Database](_.executionEvents)

  val lastExecutionEventId: Lens[Database, ExecutionEvent.Id] =
    executionEvents.andThen(Table.lastKey)

  val observations: Lens[Database, Table[Observation.Id, ObservationModel]] =
    Focus[Database](_.observations)

  val observationRows: Lens[Database, SortedMap[Observation.Id, ObservationModel]] =
    observations.andThen(Table.rows)

  val lastObservationId: Lens[Database, Observation.Id] =
    observations.andThen(Table.lastKey)

  val programs: Lens[Database, Table[Program.Id, ProgramModel]] =
    Focus[Database](_.programs)

  val lastProgramId: Lens[Database, Program.Id] =
    programs.andThen(Table.lastKey)

  val targets: Lens[Database, Table[Target.Id, TargetModel]] =
    Focus[Database](_.targets)

  val lastTargetId: Lens[Database, Target.Id] =
    targets.andThen(Table.lastKey)

}

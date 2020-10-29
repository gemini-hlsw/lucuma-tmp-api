// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{AsterismModel, ObservationModel, ProgramModel, TargetModel}
import cats.instances.order._
import monocle.Lens
import monocle.function.At

import scala.collection.immutable.{SortedMap, TreeMap}

/**
 * Simplistic immutable database "tables" of top-level types keyed by Id.
 */
final case class Tables(
  ids:              Ids,
  asterisms:        SortedMap[AsterismModel.Id, AsterismModel],
  observations:     SortedMap[ObservationModel.Id, ObservationModel],
  programs:         SortedMap[ProgramModel.Id, ProgramModel],
  targets:          SortedMap[TargetModel.Id, TargetModel],
  programAsterisms: ManyToMany[ProgramModel.Id, AsterismModel.Id],
  programTargets:   ManyToMany[ProgramModel.Id, TargetModel.Id]
)

object Tables extends TableOptics {

  val empty: Tables =
    Tables(
      ids              = Ids.zero,

      asterisms        = TreeMap.empty[AsterismModel.Id, AsterismModel],
      observations     = TreeMap.empty[ObservationModel.Id, ObservationModel],
      programs         = TreeMap.empty[ProgramModel.Id, ProgramModel],
      targets          = TreeMap.empty[TargetModel.Id, TargetModel],

      programAsterisms = ManyToMany.empty,
      programTargets   = ManyToMany.empty
    )

}

sealed trait TableOptics { self: Tables.type =>

  val ids: Lens[Tables, Ids] =
    Lens[Tables, Ids](_.ids)(b => a => a.copy(ids = b))

  val lastEventId: Lens[Tables, Long] =
    ids ^|-> Ids.lastEvent

  val lastAsterismId: Lens[Tables, AsterismModel.Id] =
    ids ^|-> Ids.lastAsterism

  val lastObservationId: Lens[Tables, ObservationModel.Id] =
    ids ^|-> Ids.lastObservation

  val lastProgramId: Lens[Tables, ProgramModel.Id] =
    ids ^|-> Ids.lastProgram

  val lastTargetId: Lens[Tables, TargetModel.Id] =
    ids ^|-> Ids.lastTarget


  val asterisms: Lens[Tables, SortedMap[AsterismModel.Id, AsterismModel]] =
    Lens[Tables, SortedMap[AsterismModel.Id, AsterismModel]](_.asterisms)(b => a => a.copy(asterisms = b))

  def asterism(aid: AsterismModel.Id): Lens[Tables, Option[AsterismModel]] =
    asterisms ^|-> At.at(aid)

  val observations: Lens[Tables, SortedMap[ObservationModel.Id, ObservationModel]] =
    Lens[Tables, SortedMap[ObservationModel.Id, ObservationModel]](_.observations)(b => a => a.copy(observations = b))

  def observation(oid: ObservationModel.Id): Lens[Tables, Option[ObservationModel]] =
    observations ^|-> At.at(oid)

  val programs: Lens[Tables, SortedMap[ProgramModel.Id, ProgramModel]] =
    Lens[Tables, SortedMap[ProgramModel.Id, ProgramModel]](_.programs)(b => a => a.copy(programs = b))

  def program(pid: ProgramModel.Id): Lens[Tables, Option[ProgramModel]] =
    programs ^|-> At.at(pid)

  val targets: Lens[Tables, SortedMap[TargetModel.Id, TargetModel]] =
    Lens[Tables, SortedMap[TargetModel.Id, TargetModel]](_.targets)(b => a => a.copy(targets = b))

  def target(tid: TargetModel.Id): Lens[Tables, Option[TargetModel]] =
    targets ^|-> At.at(tid)

  val programAsterisms: Lens[Tables, ManyToMany[ProgramModel.Id, AsterismModel.Id]] =
    Lens[Tables, ManyToMany[ProgramModel.Id, AsterismModel.Id]](_.programAsterisms)(b => a => a.copy(programAsterisms = b))

  val programTargets: Lens[Tables, ManyToMany[ProgramModel.Id, TargetModel.Id]] =
    Lens[Tables, ManyToMany[ProgramModel.Id, TargetModel.Id]](_.programTargets)(b => a => a.copy(programTargets = b))

}
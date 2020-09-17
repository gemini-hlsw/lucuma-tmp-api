// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{Asterism, Observation, Program, Target}

import cats.data.State
import cats.kernel.BoundedEnumerable
import cats.instances.order._
import cats.syntax.all._
import monocle.Lens
import monocle.function.At
import monocle.state.all._

import scala.collection.immutable.{SortedMap, TreeMap}

/**
 * Simplistic immutable database "tables" of top-level types keyed by Id.
 */
final case class Tables(
  ids:              Ids,

  asterisms:        SortedMap[Asterism.Id, Asterism],
  observations:     SortedMap[Observation.Id, Observation],
  programs:         SortedMap[Program.Id, Program],
  targets:          SortedMap[Target.Id, Target],

  programAsterisms: ManyToMany[Program.Id, Asterism.Id],
  programTargets:   ManyToMany[Program.Id, Target.Id]
)

object Tables extends TableOptics with TableState {

  val empty: Tables =
    Tables(
      ids              = Ids.zero,

      asterisms        = TreeMap.empty[Asterism.Id, Asterism],
      observations     = TreeMap.empty[Observation.Id, Observation],
      programs         = TreeMap.empty[Program.Id, Program],
      targets          = TreeMap.empty[Target.Id, Target],

      programAsterisms = ManyToMany.empty,
      programTargets   = ManyToMany.empty
    )

}

sealed trait TableOptics { self: Tables.type =>

  val ids: Lens[Tables, Ids] =
    Lens[Tables, Ids](_.ids)(b => a => a.copy(ids = b))

  val lastEventId: Lens[Tables, Long] =
    ids ^|-> Ids.lastEvent

  val lastAsterismId: Lens[Tables, Asterism.Id] =
    ids ^|-> Ids.lastAsterism

  val lastObservationId: Lens[Tables, Observation.Id] =
    ids ^|-> Ids.lastObservation

  val lastProgramId: Lens[Tables, Program.Id] =
    ids ^|-> Ids.lastProgram

  val lastTargetId: Lens[Tables, Target.Id] =
    ids ^|-> Ids.lastTarget


  val asterisms: Lens[Tables, SortedMap[Asterism.Id, Asterism]] =
    Lens[Tables, SortedMap[Asterism.Id, Asterism]](_.asterisms)(b => a => a.copy(asterisms = b))

  def asterism(aid: Asterism.Id): Lens[Tables, Option[Asterism]] =
    asterisms ^|-> At.at(aid)

  val observations: Lens[Tables, SortedMap[Observation.Id, Observation]] =
    Lens[Tables, SortedMap[Observation.Id, Observation]](_.observations)(b => a => a.copy(observations = b))

  def observation(oid: Observation.Id): Lens[Tables, Option[Observation]] =
    observations ^|-> At.at(oid)

  val programs: Lens[Tables, SortedMap[Program.Id, Program]] =
    Lens[Tables, SortedMap[Program.Id, Program]](_.programs)(b => a => a.copy(programs = b))

  def program(pid: Program.Id): Lens[Tables, Option[Program]] =
    programs ^|-> At.at(pid)

  val targets: Lens[Tables, SortedMap[Target.Id, Target]] =
    Lens[Tables, SortedMap[Target.Id, Target]](_.targets)(b => a => a.copy(targets = b))

  def target(tid: Target.Id): Lens[Tables, Option[Target]] =
    targets ^|-> At.at(tid)


  val programAsterisms: Lens[Tables, ManyToMany[Program.Id, Asterism.Id]] =
    Lens[Tables, ManyToMany[Program.Id, Asterism.Id]](_.programAsterisms)(b => a => a.copy(programAsterisms = b))

  val programTargets: Lens[Tables, ManyToMany[Program.Id, Target.Id]] =
    Lens[Tables, ManyToMany[Program.Id, Target.Id]](_.programTargets)(b => a => a.copy(programTargets = b))


}

sealed trait TableState { self: Tables.type =>

  val nextEventId: State[Tables, Long] =
    lastEventId.mod(_ + 1L)

  val nextAsterismId: State[Tables, Asterism.Id] =
    lastAsterismId.mod(BoundedEnumerable[Asterism.Id].cycleNext)

  val nextObservationId: State[Tables, Observation.Id] =
    lastObservationId.mod(BoundedEnumerable[Observation.Id].cycleNext)

  val nextProgramId: State[Tables, Program.Id] =
    lastProgramId.mod(BoundedEnumerable[Program.Id].cycleNext)

  val nextTargetId: State[Tables, Target.Id] =
    lastTargetId.mod(BoundedEnumerable[Target.Id].cycleNext)


  def shareAsterism(a: Asterism, pids: Set[Program.Id]): State[Tables, Unit] =
    programAsterisms.mod_(_ ++ pids.toList.tupleRight(a.id))

  def unshareAsterismAll(aid: Asterism.Id): State[Tables, Unit] =
    programAsterisms.mod_(_.removeRight(aid))

  def shareTarget(t: Target, pids: Set[Program.Id]): State[Tables, Unit] =
    programTargets.mod_(_ ++ pids.toList.tupleRight(t.id))

  def unshareTargetAll(tid: Target.Id): State[Tables, Unit] =
    programTargets.mod_(_.removeRight(tid))

}
// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{AsterismModel, ObservationModel, ProgramModel, TargetModel}
import lucuma.core.model.{Asterism, Observation, Program, Target}
import cats.instances.order._
import monocle.Lens
import monocle.function.At

import scala.collection.immutable.{SortedMap, TreeMap}

/**
 * Simplistic immutable database "tables" of top-level types keyed by Id.
 */
final case class Tables(
  ids:             Ids,
  asterisms:       SortedMap[Asterism.Id,    AsterismModel],
  observations:    SortedMap[Observation.Id, ObservationModel],
  programs:        SortedMap[Program.Id,     ProgramModel],
  targets:         SortedMap[Target.Id,      TargetModel],

  programAsterism: ManyToMany[Program.Id, Asterism.Id],
  programTarget:   ManyToMany[Program.Id, Target.Id],
  targetAsterism:  ManyToMany[Target.Id,  Asterism.Id]
)

object Tables extends TableOptics {

  val empty: Tables =
    Tables(
      ids              = Ids.zero,

      asterisms        = TreeMap.empty[Asterism.Id,    AsterismModel],
      observations     = TreeMap.empty[Observation.Id, ObservationModel],
      programs         = TreeMap.empty[Program.Id,     ProgramModel],
      targets          = TreeMap.empty[Target.Id,      TargetModel],

      programAsterism = ManyToMany.empty,
      programTarget   = ManyToMany.empty,
      targetAsterism  = ManyToMany.empty
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


  val asterisms: Lens[Tables, SortedMap[Asterism.Id, AsterismModel]] =
    Lens[Tables, SortedMap[Asterism.Id, AsterismModel]](_.asterisms)(b => a => a.copy(asterisms = b))

  def asterism(aid: Asterism.Id): Lens[Tables, Option[AsterismModel]] =
    asterisms ^|-> At.at(aid)


  val observations: Lens[Tables, SortedMap[Observation.Id, ObservationModel]] =
    Lens[Tables, SortedMap[Observation.Id, ObservationModel]](_.observations)(b => a => a.copy(observations = b))

  def observation(oid: Observation.Id): Lens[Tables, Option[ObservationModel]] =
    observations ^|-> At.at(oid)


  val programs: Lens[Tables, SortedMap[Program.Id, ProgramModel]] =
    Lens[Tables, SortedMap[Program.Id, ProgramModel]](_.programs)(b => a => a.copy(programs = b))

  def program(pid: Program.Id): Lens[Tables, Option[ProgramModel]] =
    programs ^|-> At.at(pid)


  val targets: Lens[Tables, SortedMap[Target.Id, TargetModel]] =
    Lens[Tables, SortedMap[Target.Id, TargetModel]](_.targets)(b => a => a.copy(targets = b))

  def target(tid: Target.Id): Lens[Tables, Option[TargetModel]] =
    targets ^|-> At.at(tid)


  val programAsterism: Lens[Tables, ManyToMany[Program.Id, Asterism.Id]] =
    Lens[Tables, ManyToMany[Program.Id, Asterism.Id]](_.programAsterism)(b => a => a.copy(programAsterism = b))

  val programTarget: Lens[Tables, ManyToMany[Program.Id, Target.Id]] =
    Lens[Tables, ManyToMany[Program.Id, Target.Id]](_.programTarget)(b => a => a.copy(programTarget = b))

  val targetAsterism: Lens[Tables, ManyToMany[Target.Id, Asterism.Id]] =
    Lens[Tables, ManyToMany[Target.Id, Asterism.Id]](_.targetAsterism)(b => a => a.copy(targetAsterism = b))

}
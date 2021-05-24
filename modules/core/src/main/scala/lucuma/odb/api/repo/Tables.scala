// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.core.model.{Asterism, Atom, ConstraintSet, Observation, Program, Step, Target}
import lucuma.odb.api.model.{AsterismModel, AtomModel, ConstraintSetModel, Dataset, DatasetModel, ExecutionEvent, ExecutionEventModel, ObservationModel, ProgramModel, StepModel, TargetModel}
import cats.instances.order._
import monocle.Lens
import monocle.function.At

import scala.collection.immutable.{SortedMap, TreeMap}

/**
 * Simplistic immutable database "tables" of top-level types keyed by Id.
 */
final case class Tables(
  ids:             Ids,
  atoms:           SortedMap[Atom.Id, AtomModel[Step.Id]],
  asterisms:       SortedMap[Asterism.Id, AsterismModel],
  constraintSets:  SortedMap[ConstraintSet.Id, ConstraintSetModel],
  datasets:        SortedMap[Dataset.Id, DatasetModel],
  executionEvents: SortedMap[ExecutionEvent.Id, ExecutionEventModel],
  observations:    SortedMap[Observation.Id, ObservationModel],
  programs:        SortedMap[Program.Id, ProgramModel],
  steps:           SortedMap[Step.Id, StepModel[_]],
  targets:         SortedMap[Target.Id, TargetModel],

  programAsterism: ManyToMany[Program.Id, Asterism.Id],
  programTarget:   ManyToMany[Program.Id, Target.Id],
  targetAsterism:  ManyToMany[Target.Id, Asterism.Id],
)

object Tables extends TableOptics {

  val empty: Tables =
    Tables(
      ids             = Ids.zero,

      atoms           = TreeMap.empty[Atom.Id, AtomModel[Step.Id]],
      asterisms       = TreeMap.empty[Asterism.Id, AsterismModel],
      constraintSets  = TreeMap.empty[ConstraintSet.Id, ConstraintSetModel],
      datasets        = TreeMap.empty[Dataset.Id, DatasetModel],
      executionEvents = TreeMap.empty[ExecutionEvent.Id, ExecutionEventModel],
      observations    = TreeMap.empty[Observation.Id, ObservationModel],
      programs        = TreeMap.empty[Program.Id, ProgramModel],
      steps           = TreeMap.empty[Step.Id, StepModel[_]],
      targets         = TreeMap.empty[Target.Id, TargetModel],

      programAsterism = ManyToMany.empty,
      programTarget   = ManyToMany.empty,
      targetAsterism  = ManyToMany.empty,
    )

}

sealed trait TableOptics { self: Tables.type =>

  val ids: Lens[Tables, Ids] =
    Lens[Tables, Ids](_.ids)(b => a => a.copy(ids = b))

  val lastEventId: Lens[Tables, Long] =
    ids ^|-> Ids.lastEvent

  val lastAtomId: Lens[Tables, Atom.Id] =
    ids ^|-> Ids.lastAtom

  val lastAsterismId: Lens[Tables, Asterism.Id] =
    ids ^|-> Ids.lastAsterism

  val lastConstraintSetId: Lens[Tables, ConstraintSet.Id] =
    ids ^|-> Ids.lastConstraintSet

  val lastDatasetId: Lens[Tables, Dataset.Id] =
    ids ^|-> Ids.lastDataset

  val lastExecutionEventId: Lens[Tables, ExecutionEvent.Id] =
    ids ^|-> Ids.lastExecutionEvent

  val lastObservationId: Lens[Tables, Observation.Id] =
    ids ^|-> Ids.lastObservation

  val lastProgramId: Lens[Tables, Program.Id] =
    ids ^|-> Ids.lastProgram

  val lastStepId: Lens[Tables, Step.Id] =
    ids ^|-> Ids.lastStep

  val lastTargetId: Lens[Tables, Target.Id] =
    ids ^|-> Ids.lastTarget

  val atoms: Lens[Tables, SortedMap[Atom.Id, AtomModel[Step.Id]]] =
    Lens[Tables, SortedMap[Atom.Id, AtomModel[Step.Id]]](_.atoms)(b => a => a.copy(atoms = b))

  def atom(aid: Atom.Id): Lens[Tables, Option[AtomModel[Step.Id]]] =
    atoms ^|-> At.at(aid)

  val asterisms: Lens[Tables, SortedMap[Asterism.Id, AsterismModel]] =
    Lens[Tables, SortedMap[Asterism.Id, AsterismModel]](_.asterisms)(b => a => a.copy(asterisms = b))

  def asterism(aid: Asterism.Id): Lens[Tables, Option[AsterismModel]] =
    asterisms ^|-> At.at(aid)

  def constraintSets: Lens[Tables, SortedMap[ConstraintSet.Id, ConstraintSetModel]] =
    Lens[Tables, SortedMap[ConstraintSet.Id, ConstraintSetModel]](_.constraintSets)(b =>
      a => a.copy(constraintSets = b)
    )

  def constraintSet(csid: ConstraintSet.Id): Lens[Tables, Option[ConstraintSetModel]] =
    constraintSets ^|-> At.at(csid)

  val datasets: Lens[Tables, SortedMap[Dataset.Id, DatasetModel]] =
    Lens[Tables, SortedMap[Dataset.Id, DatasetModel]](_.datasets)(b => a => a.copy(datasets = b))

  def dataset(did: Dataset.Id): Lens[Tables, Option[DatasetModel]] =
    datasets ^|-> At.at(did)

  val executionEvents: Lens[Tables, SortedMap[ExecutionEvent.Id, ExecutionEventModel]] =
    Lens[Tables, SortedMap[ExecutionEvent.Id, ExecutionEventModel]](_.executionEvents)(b => a => a.copy(executionEvents = b))

  def executionEvent(eid: ExecutionEvent.Id): Lens[Tables, Option[ExecutionEventModel]] =
    executionEvents ^|-> At.at(eid)

  val observations: Lens[Tables, SortedMap[Observation.Id, ObservationModel]] =
    Lens[Tables, SortedMap[Observation.Id, ObservationModel]](_.observations)(b => a => a.copy(observations = b))

  def observation(oid: Observation.Id): Lens[Tables, Option[ObservationModel]] =
    observations ^|-> At.at(oid)

  val programs: Lens[Tables, SortedMap[Program.Id, ProgramModel]] =
    Lens[Tables, SortedMap[Program.Id, ProgramModel]](_.programs)(b => a => a.copy(programs = b))

  def program(pid: Program.Id): Lens[Tables, Option[ProgramModel]] =
    programs ^|-> At.at(pid)

  val steps: Lens[Tables, SortedMap[Step.Id, StepModel[_]]] =
    Lens[Tables, SortedMap[Step.Id, StepModel[_]]](_.steps)(b => a => a.copy(steps = b))

  def step(sid: Step.Id): Lens[Tables, Option[StepModel[_]]] =
    steps ^|-> At.at(sid)

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

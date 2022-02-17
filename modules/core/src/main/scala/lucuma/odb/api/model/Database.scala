// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.model.{Atom, ExecutionEvent, Observation, Program, Step, Target}
import lucuma.odb.api.model.targetModel.TargetModel
import monocle.{Focus, Lens}

final case class Database(
  atoms:           Table[Atom.Id, AtomModel[Step.Id]],
  executionEvents: Table[ExecutionEvent.Id, ExecutionEventModel],
  observations:    Table[Observation.Id, ObservationModel],
  programs:        Table[Program.Id, ProgramModel],
  steps:           Table[Step.Id, StepModel[_]],
  targets:         Table[Target.Id, TargetModel]
)

object Database extends DatabaseOptics {

  val empty: Database =
    Database(
      atoms           = Table.empty,
      executionEvents = Table.empty,
      observations    = Table.empty,
      programs        = Table.empty,
      steps           = Table.empty,
      targets         = Table.empty
    )

  val atom: DatabaseState[Atom.Id, AtomModel[Step.Id]] =
    DatabaseState(atoms)

  val executionEvent: DatabaseState[ExecutionEvent.Id, ExecutionEventModel] =
    DatabaseState(executionEvents)

  val observation: DatabaseState[Observation.Id, ObservationModel] =
    DatabaseState(observations)

  val program: DatabaseState[Program.Id, ProgramModel] =
    DatabaseState(programs)

  val step: DatabaseState[Step.Id, StepModel[_]] =
    DatabaseState(steps)

  val target: DatabaseState[Target.Id, TargetModel] =
    DatabaseState(targets)

}

sealed trait DatabaseOptics { self: Database.type =>

  val atoms: Lens[Database, Table[Atom.Id, AtomModel[Step.Id]]] =
    Focus[Database](_.atoms)

  val lastAtomId: Lens[Database, Atom.Id] =
    atoms.andThen(Table.lastKey)

  val executionEvents: Lens[Database, Table[ExecutionEvent.Id, ExecutionEventModel]] =
    Focus[Database](_.executionEvents)

  val lastExecutionEventId: Lens[Database, ExecutionEvent.Id] =
    executionEvents.andThen(Table.lastKey)

  val observations: Lens[Database, Table[Observation.Id, ObservationModel]] =
    Focus[Database](_.observations)

  val lastObservationId: Lens[Database, Observation.Id] =
    observations.andThen(Table.lastKey)

  val programs: Lens[Database, Table[Program.Id, ProgramModel]] =
    Focus[Database](_.programs)

  val lastProgramId: Lens[Database, Program.Id] =
    programs.andThen(Table.lastKey)

  val steps: Lens[Database, Table[Step.Id, StepModel[_]]] =
    Focus[Database](_.steps)

  val lastStepId: Lens[Database, Step.Id] =
    steps.andThen(Table.lastKey)

  val targets: Lens[Database, Table[Target.Id, TargetModel]] =
    Focus[Database](_.targets)

  val lastTargetId: Lens[Database, Target.Id] =
    targets.andThen(Table.lastKey)

}

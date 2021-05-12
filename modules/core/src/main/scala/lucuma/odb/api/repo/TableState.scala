// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.core.model.{Asterism, Atom, ConstraintSet, Observation, Program, Step, Target}
import lucuma.odb.api.model.{AsterismModel, AtomModel, ConstraintSetModel, Database, ObservationModel, ProgramModel, StepModel, Store, TargetModel}

import cats.data.State
import monocle.state.all._

trait TableState extends Database[Tables] {

  val nextEventId: State[Tables, Long] =
    Tables.lastEventId.mod(_ + 1L)

  override val atom: Store[Tables, Atom.Id, AtomModel[_]] =
    new Store("atom", Tables.lastAtomId, Tables.atoms)

  override val asterism: Store[Tables, Asterism.Id, AsterismModel] =
    new Store("asterism", Tables.lastAsterismId, Tables.asterisms)

  override val constraintSet: Store[Tables, ConstraintSet.Id, ConstraintSetModel] =
    new Store("constraintSet", Tables.lastConstraintSetId, Tables.constraintSets)

  override val observation: Store[Tables, Observation.Id, ObservationModel] =
    new Store("observation", Tables.lastObservationId, Tables.observations)

  override val program: Store[Tables, Program.Id, ProgramModel] =
    new Store("program", Tables.lastProgramId, Tables.programs)

  override val step: Store[Tables, Step.Id, StepModel[_]] =
    new Store("step", Tables.lastStepId, Tables.steps)

  override val target: Store[Tables, Target.Id, TargetModel] =
    new Store("target", Tables.lastTargetId, Tables.targets)

}

object TableState extends TableState

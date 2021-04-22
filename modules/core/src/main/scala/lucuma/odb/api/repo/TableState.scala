// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{AsterismModel, Atom, AtomModel, ConstraintSetModel, Database, ObservationModel, ProgramModel, Step, StepModel, Store, TargetModel}
import lucuma.core.model.{Asterism, ConstraintSet, Observation, Program, Target}
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

  /*
  val nextAsterismId: State[Tables, Asterism.Id] =
    Tables.lastAsterismId.mod(BoundedEnumerable[Asterism.Id].cycleNext)

  val nextConstraintSetId: State[Tables, ConstraintSet.Id] =
    Tables.lastConstraintSetId.mod(BoundedEnumerable[ConstraintSet.Id].cycleNext)

  val nextObservationId: State[Tables, Observation.Id] =
    Tables.lastObservationId.mod(BoundedEnumerable[Observation.Id].cycleNext)

  val nextProgramId: State[Tables, Program.Id] =
    Tables.lastProgramId.mod(BoundedEnumerable[Program.Id].cycleNext)

  val nextTargetId: State[Tables, Target.Id] =
    Tables.lastTargetId.mod(BoundedEnumerable[Target.Id].cycleNext)

  private def tryFind[I: Gid, T](name: String, id: I, lens: I => Lens[Tables, Option[T]]): State[Tables, ValidatedInput[T]] =
    lens(id).st.map(_.toValidNec(InputError.missingReference(name, Gid[I].show(id))))

  def asterism(aid: Asterism.Id): State[Tables, ValidatedInput[AsterismModel]] =
    tryFind("asterism", aid, Tables.asterism)

    def constraintSet(csid: ConstraintSet.Id): State[Tables, ValidatedInput[ConstraintSetModel]] =
    tryFind("constraintSet", csid, Tables.constraintSet)

  def observation(oid: Observation.Id): State[Tables, ValidatedInput[ObservationModel]] =
    tryFind("observation", oid, Tables.observation)

  def program(pid: Program.Id): State[Tables, ValidatedInput[ProgramModel]] =
    tryFind("program", pid, Tables.program)

  def target(tid: Target.Id): State[Tables, ValidatedInput[TargetModel]] =
    tryFind("target", tid, Tables.target)


  private def require[A](s: State[Tables, ValidatedInput[A]]): State[Tables, A] =
    s.map(_.valueOr(nec => throw InputError.Exception(nec)))

  def requireAsterism(aid: Asterism.Id): State[Tables, AsterismModel] =
    require(asterism(aid))

  def requireConstraintSet(csid: ConstraintSet.Id): State[Tables, ConstraintSetModel] =
    require(constraintSet(csid))

  def requireObservation(oid: Observation.Id): State[Tables, ObservationModel] =
    require(observation(oid))

  def requireProgram(pid: Program.Id): State[Tables, ProgramModel] =
    require(program(pid))

  def requireTarget(tid: Target.Id): State[Tables, TargetModel] =
    require(target(tid))
 */
}

object TableState extends TableState

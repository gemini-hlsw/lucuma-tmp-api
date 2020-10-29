// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.core.util.Gid
import lucuma.odb.api.model.{AsterismModel, InputError, ObservationModel, ProgramModel, TargetModel, ValidatedInput}

import cats.data.State
import cats.kernel.BoundedEnumerable
import cats.syntax.functor._
import cats.syntax.option._
import monocle.Lens
import monocle.state.all._

trait TableState {

  val nextEventId: State[Tables, Long] =
    Tables.lastEventId.mod(_ + 1L)

  val nextAsterismId: State[Tables, AsterismModel.Id] =
    Tables.lastAsterismId.mod(BoundedEnumerable[AsterismModel.Id].cycleNext)

  val nextObservationId: State[Tables, ObservationModel.Id] =
    Tables.lastObservationId.mod(BoundedEnumerable[ObservationModel.Id].cycleNext)

  val nextProgramId: State[Tables, ProgramModel.Id] =
    Tables.lastProgramId.mod(BoundedEnumerable[ProgramModel.Id].cycleNext)

  val nextTargetId: State[Tables, TargetModel.Id] =
    Tables.lastTargetId.mod(BoundedEnumerable[TargetModel.Id].cycleNext)

  def shareAsterismWithPrograms(a: AsterismModel, pids: Set[ProgramModel.Id]): State[Tables, Unit] =
    Tables.programAsterisms.mod_(_ ++ pids.toList.tupleRight(a.id))

  def unshareAsterismWithPrograms(a: AsterismModel, pids: Set[ProgramModel.Id]): State[Tables, Unit] =
    Tables.programAsterisms.mod_(_ -- pids.toList.tupleRight(a.id))

  def unshareAsterismAll(aid: AsterismModel.Id): State[Tables, Unit] =
    Tables.programAsterisms.mod_(_.removeRight(aid))

  def shareTargetWithPrograms(t: TargetModel, pids: Set[ProgramModel.Id]): State[Tables, Unit] =
    Tables.programTargets.mod_(_ ++ pids.toList.tupleRight(t.id))

  def unshareTargetWithPrograms(t: TargetModel, pids: Set[ProgramModel.Id]): State[Tables, Unit] =
    Tables.programTargets.mod_(_ -- pids.toList.tupleRight(t.id))

  def unshareTargetAll(tid: TargetModel.Id): State[Tables, Unit] =
    Tables.programTargets.mod_(_.removeRight(tid))


  private def tryFind[I: Gid, T](name: String, id: I, lens: I => Lens[Tables, Option[T]]): State[Tables, ValidatedInput[T]] =
    lens(id).st.map(_.toValidNec(InputError.missingReference(name, Gid[I].show(id))))

  def asterism(aid: AsterismModel.Id): State[Tables, ValidatedInput[AsterismModel]] =
    tryFind("asterism", aid, Tables.asterism)

  def observation(oid: ObservationModel.Id): State[Tables, ValidatedInput[ObservationModel]] =
    tryFind("observation", oid, Tables.observation)

  def program(pid: ProgramModel.Id): State[Tables, ValidatedInput[ProgramModel]] =
    tryFind("program", pid, Tables.program)

  def target(tid: TargetModel.Id): State[Tables, ValidatedInput[TargetModel]] =
    tryFind("target", tid, Tables.target)


  private def require[A](s: State[Tables, ValidatedInput[A]]): State[Tables, A] =
    s.map(_.valueOr(nec => throw InputError.Exception(nec)))

  def requireAsterism(aid: AsterismModel.Id): State[Tables, AsterismModel] =
    require(asterism(aid))

  def requireObservation(oid: ObservationModel.Id): State[Tables, ObservationModel] =
    require(observation(oid))

  def requireProgram(pid: ProgramModel.Id): State[Tables, ProgramModel] =
    require(program(pid))

  def requireTarget(tid: TargetModel.Id): State[Tables, TargetModel] =
    require(target(tid))

}

object TableState extends TableState

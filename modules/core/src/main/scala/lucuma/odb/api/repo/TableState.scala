// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.core.util.Gid
import lucuma.odb.api.model.{AsterismModel, InputError, ObservationModel, ProgramModel, TargetModel, ValidatedInput}
import lucuma.core.model.{Asterism, Observation, Program, Target}

import cats.data.State
import cats.kernel.BoundedEnumerable
import cats.syntax.option._
import monocle.Lens
import monocle.state.all._

trait TableState {

  val nextEventId: State[Tables, Long] =
    Tables.lastEventId.mod(_ + 1L)

  val nextAsterismId: State[Tables, Asterism.Id] =
    Tables.lastAsterismId.mod(BoundedEnumerable[Asterism.Id].cycleNext)

  val nextObservationId: State[Tables, Observation.Id] =
    Tables.lastObservationId.mod(BoundedEnumerable[Observation.Id].cycleNext)

  val nextProgramId: State[Tables, Program.Id] =
    Tables.lastProgramId.mod(BoundedEnumerable[Program.Id].cycleNext)

  val nextTargetId: State[Tables, Target.Id] =
    Tables.lastTargetId.mod(BoundedEnumerable[Target.Id].cycleNext)

/*
  private def share[A, B: Order](a: A, bs: Set[B], lens: Lens[Tables, SortedMap[B, _]]): State[Tables, Unit] =
    lens.mod_(_ ++ bs.toList.tupleRight(a))

  private def unshare[A, B: Order](a: A, bs: Set[B], lens: Lens[Tables, SortedMap[B, _]]): State[Tables, Unit] =
    lens.mod_(_ -- bs.toList.tupleRight(a))

  def shareAsterismWithPrograms(a: AsterismModel, pids: Set[Program.Id]): State[Tables, Unit] =
//    Tables.programAsterisms.mod_(_ ++ pids.toList.tupleRight(a.id))
    share(a.id, pids, Tables.programAsterism)

  def unshareAsterismWithPrograms(a: AsterismModel, pids: Set[Program.Id]): State[Tables, Unit] =
//    Tables.programAsterisms.mod_(_ -- pids.toList.tupleRight(a.id))
    unshare(a.id, pids, Tables.programAsterism)

  def shareAsterismWithTargets(a: AsModel, aids: Set[Asterism.Id]): State[Tables, Unit] =
    share(t.id, aids, Tables.targetAsterism)

  def unshareTargetWithAsterisms(t: TargetModel, aids: Set[Asterism.Id]): State[Tables, Unit] =
    unshare(t.id, aids, Tables.targetAsterism)

  def unshareAsterismAll(aid: Asterism.Id): State[Tables, Unit] =
    for {
      _ <- Tables.programAsterism.mod_(_.removeRight(aid))
      _ <- Tables.targetAsterism.mod_(_.removeLeft(aid))
    } yield ()

  def shareTargetWithPrograms(t: TargetModel, pids: Set[Program.Id]): State[Tables, Unit] =
    Tables.programTarget.mod_(_ ++ pids.toList.tupleRight(t.id))

  def unshareTargetWithPrograms(t: TargetModel, pids: Set[Program.Id]): State[Tables, Unit] =
    Tables.programTarget.mod_(_ -- pids.toList.tupleRight(t.id))

  def unshareTargetAll(tid: Target.Id): State[Tables, Unit] =
    Tables.programTarget.mod_(_.removeRight(tid))
*/

  private def tryFind[I: Gid, T](name: String, id: I, lens: I => Lens[Tables, Option[T]]): State[Tables, ValidatedInput[T]] =
    lens(id).st.map(_.toValidNec(InputError.missingReference(name, Gid[I].show(id))))

  def asterism(aid: Asterism.Id): State[Tables, ValidatedInput[AsterismModel]] =
    tryFind("asterism", aid, Tables.asterism)

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

  def requireObservation(oid: Observation.Id): State[Tables, ObservationModel] =
    require(observation(oid))

  def requireProgram(pid: Program.Id): State[Tables, ProgramModel] =
    require(program(pid))

  def requireTarget(tid: Target.Id): State[Tables, TargetModel] =
    require(target(tid))

}

object TableState extends TableState

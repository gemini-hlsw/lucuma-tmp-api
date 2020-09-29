// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{AsterismModel, ObservationModel, ProgramModel, TargetModel, ValidatedInput}
import lucuma.odb.api.model.InputError
import lucuma.core.util.Gid
import cats._
import cats.data.State
import cats.implicits._

import scala.collection.immutable.SortedMap

trait LookupSupport[F[_]] {

  def missingReference[I: Gid, T](id: I)(implicit M: MonadError[F, Throwable]): F[T] =
    ExecutionException.missingReference[F, I, T](id)

  def lookup[I: Gid, T](m: SortedMap[I, T], id: I, name: String): ValidatedInput[T] =
    m.get(id).toValidNec(InputError.missingReference(name, Gid[I].show(id)))

  def lookupAsterism(t: Tables, aid: AsterismModel.Id): ValidatedInput[AsterismModel] =
    lookup(t.asterisms, aid, "aid")

  def lookupObservation(t: Tables, oid: ObservationModel.Id): ValidatedInput[ObservationModel] =
    lookup(t.observations, oid, "oid")

  def lookupProgram(t: Tables, pid: ProgramModel.Id): ValidatedInput[ProgramModel] =
    lookup(t.programs, pid, "pid")

  def lookupTarget(t: Tables, tid: TargetModel.Id): ValidatedInput[TargetModel] =
    lookup(t.targets, tid, "tid")

  def inspectAsterismId(aid: AsterismModel.Id): State[Tables, ValidatedInput[AsterismModel]] =
    State.inspect(lookupAsterism(_, aid))

  def inspectObservationId(oid: ObservationModel.Id): State[Tables, ValidatedInput[ObservationModel]] =
    State.inspect(lookupObservation(_, oid))

  def inspectProgramId(pid: ProgramModel.Id): State[Tables, ValidatedInput[ProgramModel]] =
    State.inspect(lookupProgram(_, pid))

  def inspectTargetId(tid: TargetModel.Id): State[Tables, ValidatedInput[TargetModel]] =
    State.inspect(lookupTarget(_, tid))

}

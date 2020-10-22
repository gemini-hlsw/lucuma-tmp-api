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

  /**
   * Verify that the given id, if supplied, does not exist in the map.
   */
  def dontFind[I: Gid, T](m: SortedMap[I, T], id: Option[I], name: String): ValidatedInput[Unit] =
    id.fold(().validNec[InputError]) { i =>
      m.get(i).as(InputError.idClash(name, Gid[I].show(i))).toInvalidNec(())
    }

  def lookupAsterism(t: Tables, aid: AsterismModel.Id): ValidatedInput[AsterismModel] =
    lookup(t.asterisms, aid, "asterism")

  def dontFindAsterism(t: Tables, aid: Option[AsterismModel.Id]): ValidatedInput[Unit] =
    dontFind(t.asterisms, aid, "asterism")

  def lookupObservation(t: Tables, oid: ObservationModel.Id): ValidatedInput[ObservationModel] =
    lookup(t.observations, oid, "observation")

  def dontFindObservation(t: Tables, oid: Option[ObservationModel.Id]): ValidatedInput[Unit] =
    dontFind(t.observations, oid, "observation")

  def lookupProgram(t: Tables, pid: ProgramModel.Id): ValidatedInput[ProgramModel] =
    lookup(t.programs, pid, "program")

  def dontFindProgram(t: Tables, pid: Option[ProgramModel.Id]): ValidatedInput[Unit] =
    dontFind(t.programs, pid, "program")

  def lookupTarget(t: Tables, tid: TargetModel.Id): ValidatedInput[TargetModel] =
    lookup(t.targets, tid, "target")

  def dontFindTarget(t: Tables, tid: Option[TargetModel.Id]): ValidatedInput[Unit] =
    dontFind(t.targets, tid, "target")

  def inspectAsterismId(aid: AsterismModel.Id): State[Tables, ValidatedInput[AsterismModel]] =
    State.inspect(lookupAsterism(_, aid))

  def inspectObservationId(oid: ObservationModel.Id): State[Tables, ValidatedInput[ObservationModel]] =
    State.inspect(lookupObservation(_, oid))

  def inspectProgramId(pid: ProgramModel.Id): State[Tables, ValidatedInput[ProgramModel]] =
    State.inspect(lookupProgram(_, pid))

  def inspectTargetId(tid: TargetModel.Id): State[Tables, ValidatedInput[TargetModel]] =
    State.inspect(lookupTarget(_, tid))

}

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{AsterismModel, ObservationModel, ProgramModel, TargetModel, ValidatedInput}
import lucuma.odb.api.model.InputError
import lucuma.core.util.Gid
import cats._
import cats.implicits._

import scala.collection.immutable.SortedMap

trait LookupSupport[F[_]] {

  def missingAsterism(aid: AsterismModel.Id)(implicit M: MonadError[F, Throwable]): F[AsterismModel] =
    ExecutionException.missingReference[F, AsterismModel.Id, AsterismModel](aid)

  def missingObservation(oid: ObservationModel.Id)(implicit M: MonadError[F, Throwable]): F[ObservationModel] =
    ExecutionException.missingReference[F, ObservationModel.Id, ObservationModel](oid)

  def missingProgram(pid: ProgramModel.Id)(implicit M: MonadError[F, Throwable]): F[ProgramModel] =
    ExecutionException.missingReference[F, ProgramModel.Id, ProgramModel](pid)

  def missingTarget(tid: TargetModel.Id)(implicit M: MonadError[F, Throwable]): F[TargetModel] =
    ExecutionException.missingReference[F, TargetModel.Id, TargetModel](tid)

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

}

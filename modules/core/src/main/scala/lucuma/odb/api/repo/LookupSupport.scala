// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{Asterism, Observation, Program, Target, ValidatedInput}
import lucuma.odb.api.model.InputError
import lucuma.core.util.Gid
import cats._
import cats.implicits._

import scala.collection.immutable.SortedMap

trait LookupSupport[F[_]] {

  def missingAsterism(aid: Asterism.Id)(implicit M: MonadError[F, Throwable]): F[Asterism] =
    ExecutionException.missingReference[F, Asterism.Id, Asterism](aid)

  def missingObservation(oid: Observation.Id)(implicit M: MonadError[F, Throwable]): F[Observation] =
    ExecutionException.missingReference[F, Observation.Id, Observation](oid)

  def missingProgram(pid: Program.Id)(implicit M: MonadError[F, Throwable]): F[Program] =
    ExecutionException.missingReference[F, Program.Id, Program](pid)

  def missingTarget(tid: Target.Id)(implicit M: MonadError[F, Throwable]): F[Target] =
    ExecutionException.missingReference[F, Target.Id, Target](tid)

  def lookup[I: Gid, T](m: SortedMap[I, T], id: I, name: String): ValidatedInput[T] =
    m.get(id).toValidNec(InputError.missingReference(name, Gid[I].show(id)))

  def lookupAsterism(t: Tables, aid: Asterism.Id): ValidatedInput[Asterism] =
    lookup(t.asterisms, aid, "aid")

  def lookupObservation(t: Tables, oid: Observation.Id): ValidatedInput[Observation] =
    lookup(t.observations, oid, "oid")

  def lookupProgram(t: Tables, pid: Program.Id): ValidatedInput[Program] =
    lookup(t.programs, pid, "pid")

  def lookupTarget(t: Tables, tid: Target.Id): ValidatedInput[Target] =
    lookup(t.targets, tid, "tid")

}

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{
  AsterismModel,
  InputError,
  ObservationModel,
  ProgramModel,
  TargetModel,
  ValidatedInput
}
import lucuma.core.model.{ Asterism, Observation, Program, Target }
import lucuma.core.util.Gid
import cats.implicits._

import scala.collection.immutable.SortedMap

trait LookupSupport {

  def tryFind[I: Gid, T](m: SortedMap[I, T], id: I, name: String): ValidatedInput[T] =
    m.get(id).toValidNec(InputError.missingReference(name, Gid[I].show(id)))

  def tryFindAsterism(t: Tables, aid: Asterism.Id): ValidatedInput[AsterismModel] =
    tryFind(t.asterisms, aid, "asterism")

  def tryFindObservation(t: Tables, oid: Observation.Id): ValidatedInput[ObservationModel] =
    tryFind(t.observations, oid, "observation")

  def tryFindProgram(t: Tables, pid: Program.Id): ValidatedInput[ProgramModel] =
    tryFind(t.programs, pid, "program")

  def tryFindTarget(t: Tables, tid: Target.Id): ValidatedInput[TargetModel] =
    tryFind(t.targets, tid, "target")


  /**
   * Verify that the given id, if supplied, does not exist in the map.
   */
  def tryNotFind[I: Gid, T](m: SortedMap[I, T], id: Option[I], name: String): ValidatedInput[Unit] =
    id.fold(().validNec[InputError]) { i =>
      m.get(i).as(InputError.idClash(name, Gid[I].show(i))).toInvalidNec(())
    }

  def tryNotFindAsterism(t: Tables, aid: Option[Asterism.Id]): ValidatedInput[Unit] =
    tryNotFind(t.asterisms, aid, "asterism")

  def tryNotFindObservation(t: Tables, oid: Option[Observation.Id]): ValidatedInput[Unit] =
    tryNotFind(t.observations, oid, "observation")

  def tryNotFindProgram(t: Tables, pid: Option[Program.Id]): ValidatedInput[Unit] =
    tryNotFind(t.programs, pid, "program")

  def tryNotFindTarget(t: Tables, tid: Option[Target.Id]): ValidatedInput[Unit] =
    tryNotFind(t.targets, tid, "target")

}

object LookupSupport extends LookupSupport

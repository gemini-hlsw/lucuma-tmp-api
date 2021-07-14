// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{
  InputError,
  ObservationModel,
  ProgramModel,
  ValidatedInput
}
import lucuma.core.model.{ Observation, Program }
import lucuma.core.util.Gid
import cats.implicits._

import scala.collection.immutable.SortedMap

trait LookupSupport {

  def tryFind[I: Gid, T](m: SortedMap[I, T], id: I, name: String): ValidatedInput[T] =
    m.get(id).toValidNec(InputError.missingReference(name, Gid[I].show(id)))

  def tryFindObservation(t: Tables, oid: Observation.Id): ValidatedInput[ObservationModel] =
    tryFind(t.observations, oid, "observation")

  def tryFindProgram(t: Tables, pid: Program.Id): ValidatedInput[ProgramModel] =
    tryFind(t.programs, pid, "program")

  /**
   * Verify that the given id, if supplied, does not exist in the map.
   */
  def tryNotFind[I: Gid, T](m: SortedMap[I, T], id: Option[I], name: String): ValidatedInput[Unit] =
    id.fold(().validNec[InputError]) { i =>
      m.get(i).as(InputError.idClash(name, Gid[I].show(i))).toInvalidNec(())
    }

  def tryNotFindObservation(t: Tables, oid: Option[Observation.Id]): ValidatedInput[Unit] =
    tryNotFind(t.observations, oid, "observation")

  def tryNotFindProgram(t: Tables, pid: Option[Program.Id]): ValidatedInput[Unit] =
    tryNotFind(t.programs, pid, "program")

}

object LookupSupport extends LookupSupport

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.util.Gid

import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.PosLong
import monocle.Lens


/**
 * A placeholder Program for now.
 */
final case class Program(
  id:        Program.Id,
  existence: Existence,
  name:      Option[String]
)

object Program extends ProgramOptics {

  final case class Id(value: PosLong) {

    override def toString: String =
      Gid[Id].show(this)
  }

  object Id {
    implicit val GidProgramId: Gid[Id] =
      Gid.instance('p', _.value, apply)
  }

  implicit val TopLevelProgram: TopLevel[Id, Program] =
    TopLevel.instance(_.id, Program.existence)

  /**
   * Program creation input class.
   */
  final case class Create(
    name: Option[String]
  )

  final case class ProgramCreatedEvent (
    id: Long,
    value: Program,
  ) extends Event.Created[Program]

  object ProgramCreatedEvent {
    def apply(value: Program)(id: Long): ProgramCreatedEvent =
      ProgramCreatedEvent(id, value)
  }

  final case class ProgramEditedEvent (
    id: Long,
    oldValue: Program,
    newValue: Program
  ) extends Event.Edited[Program]

  object ProgramEditedEvent {
    def apply(oldValue: Program, newValue: Program)(id: Long): ProgramEditedEvent =
      ProgramEditedEvent(id, oldValue, newValue)
  }


}

trait ProgramOptics { self: Program.type =>

  val id: Lens[Program, Program.Id] =
    Lens[Program, Program.Id](_.id)(a => b => b.copy(id = a))

  val existence: Lens[Program, Existence] =
    Lens[Program, Existence](_.existence)(a => b => b.copy(existence = a))

  val name: Lens[Program, Option[String]] =
    Lens[Program, Option[String]](_.name)(a => b => b.copy(name = a))

}
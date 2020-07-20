// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.Existence._
import lucuma.odb.api.model.syntax.all._
import lucuma.core.util.Gid

import cats.data.State
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.PosLong
import monocle.Lens


final case class Observation(
  id:        Observation.Id,
  existence: Existence,
  pid:       Program.Id,
  name:      Option[String],
  asterism:  Option[Asterism.Id]
)

object Observation extends ObservationOptics {

  final case class Id(value: PosLong) {
    override def toString: String =
      Gid[Id].show(this)
  }

  object Id {
    implicit val GidObservationId: Gid[Id] =
      Gid.instance('o', _.value, apply)
  }

  implicit val TopLevelObservation: TopLevel[Id, Observation] =
    TopLevel.instance(_.id, Observation.existence)

  final case class Create(
    pid:      Program.Id,
    name:     Option[String],
    asterism: Option[Asterism.Id]
  ) {

    def withId(oid: Observation.Id): Observation =
      Observation(oid, Present, pid, name, asterism)

  }

  final case class Edit(
    id:        Observation.Id,
    existence: Option[Existence],
    name:      Option[Option[String]],
    asterism:  Option[Option[Asterism.Id]]
  ) extends Editor[Id, Observation] {

    override def editor: State[Observation, Unit] =
      for {
        _ <- Observation.existence := existence
        _ <- Observation.name      := name
        _ <- Observation.asterism  := asterism
      } yield ()
  }

  final case class ObservationCreatedEvent (
    id: Long,
    value: Observation,
  ) extends Event.Created[Observation]

  object ObservationCreatedEvent {
    def apply(value: Observation)(id: Long): ObservationCreatedEvent =
      ObservationCreatedEvent(id, value)
  }

  final case class ObservationEditedEvent (
    id: Long,
    oldValue: Observation,
    newValue: Observation
  ) extends Event.Edited[Observation]

  object ObservationEditedEvent {
    def apply(oldValue: Observation, newValue: Observation)(id: Long): ObservationEditedEvent =
      ObservationEditedEvent(id, oldValue, newValue)
  }


}

trait ObservationOptics { self: Observation.type =>

  val id: Lens[Observation, Observation.Id] =
    Lens[Observation, Observation.Id](_.id)(a => b => b.copy(id = a))

  val existence: Lens[Observation, Existence] =
    Lens[Observation, Existence](_.existence)(a => b => b.copy(existence = a))

  val name: Lens[Observation, Option[String]] =
    Lens[Observation, Option[String]](_.name)(a => b => b.copy(name = a))

  val asterism: Lens[Observation, Option[Asterism.Id]] =
    Lens[Observation, Option[Asterism.Id]](_.asterism)(a => b => b.copy(asterism = a))

}

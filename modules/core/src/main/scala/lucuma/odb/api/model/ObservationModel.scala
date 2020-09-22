// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.Existence._
import lucuma.odb.api.model.syntax.all._
import lucuma.core.util.Gid

import cats.data.State
import cats.syntax.validated._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.PosLong
import io.circe.Decoder
import io.circe.generic.semiauto._
import monocle.Lens


final case class ObservationModel(
  id:        ObservationModel.Id,
  existence: Existence,
  pid:       ProgramModel.Id,
  name:      Option[String],
  asterism:  Option[AsterismModel.Id]
)

object ObservationModel extends ObservationOptics {

  final case class Id(value: PosLong) {
    override def toString: String =
      Gid[Id].show(this)
  }

  object Id {
    implicit val GidObservationId: Gid[Id] =
      Gid.instance('o', _.value, apply)
  }

  implicit val TopLevelObservation: TopLevelModel[Id, ObservationModel] =
    TopLevelModel.instance(_.id, ObservationModel.existence)

  final case class Create(
    pid:      ProgramModel.Id,
    name:     Option[String],
    asterism: Option[AsterismModel.Id]
  ) {

    def withId(oid: ObservationModel.Id): ObservationModel =
      ObservationModel(oid, Present, pid, name, asterism)

  }

  object Create {

    implicit val DecoderCreate: Decoder[Create] =
      deriveDecoder[Create]

  }

  final case class Edit(
    id:        ObservationModel.Id,
    existence: Option[Existence],
    name:      Option[Option[String]],
    asterism:  Option[Option[AsterismModel.Id]]
  ) extends Editor[Id, ObservationModel] {

    override def editor: ValidatedInput[State[ObservationModel, Unit]] =
      (for {
        _ <- ObservationModel.existence := existence
        _ <- ObservationModel.name      := name
        _ <- ObservationModel.asterism  := asterism
      } yield ()).validNec

  }

  object Edit {

    implicit val DecoderEdit: Decoder[Edit] =
      deriveDecoder[Edit]

  }

  final case class ObservationCreatedEvent (
    id:    Long,
    value: ObservationModel,
  ) extends Event.Created[ObservationModel]

  object ObservationCreatedEvent {
    def apply(value: ObservationModel)(id: Long): ObservationCreatedEvent =
      ObservationCreatedEvent(id, value)
  }

  final case class ObservationEditedEvent (
    id:       Long,
    oldValue: ObservationModel,
    newValue: ObservationModel
  ) extends Event.Edited[ObservationModel]

  object ObservationEditedEvent {
    def apply(oldValue: ObservationModel, newValue: ObservationModel)(id: Long): ObservationEditedEvent =
      ObservationEditedEvent(id, oldValue, newValue)
  }


}

trait ObservationOptics { self: ObservationModel.type =>

  val id: Lens[ObservationModel, ObservationModel.Id] =
    Lens[ObservationModel, ObservationModel.Id](_.id)(a => b => b.copy(id = a))

  val existence: Lens[ObservationModel, Existence] =
    Lens[ObservationModel, Existence](_.existence)(a => b => b.copy(existence = a))

  val name: Lens[ObservationModel, Option[String]] =
    Lens[ObservationModel, Option[String]](_.name)(a => b => b.copy(name = a))

  val asterism: Lens[ObservationModel, Option[AsterismModel.Id]] =
    Lens[ObservationModel, Option[AsterismModel.Id]](_.asterism)(a => b => b.copy(asterism = a))

}

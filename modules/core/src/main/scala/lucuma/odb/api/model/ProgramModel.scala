// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.model.Program
import cats.Eq
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.refined._
import monocle.Lens


/**
 * A placeholder Program for now.
 */
final case class ProgramModel(
  id:        Program.Id,
  existence: Existence,
  name:      Option[NonEmptyString]
)

object ProgramModel extends ProgramOptics {

  implicit val TopLevelProgram: TopLevelModel[Program.Id, ProgramModel] =
    TopLevelModel.instance(_.id, ProgramModel.existence)

  implicit val EqProgram: Eq[ProgramModel] =
    Eq.by(p => (p.id, p.existence, p.name))

  /**
   * Program creation input class.
   */
  final case class Create(
    programId: Option[Program.Id],
    name:      Option[NonEmptyString]
  )

  object Create {

    implicit val DecoderCreate: Decoder[Create] =
      deriveDecoder[Create]

  }

  final case class ProgramEvent (
    id:       Long,
    editType: Event.EditType,
    value:    ProgramModel,
  ) extends Event.Edit[ProgramModel]

  object ProgramEvent {
    def created(value: ProgramModel)(id: Long): ProgramEvent =
      ProgramEvent(id, Event.EditType.Created, value)

    def updated(value: ProgramModel)(id: Long): ProgramEvent =
      ProgramEvent(id, Event.EditType.Updated, value)
  }


}

trait ProgramOptics { self: ProgramModel.type =>

  val id: Lens[ProgramModel, Program.Id] =
    Lens[ProgramModel, Program.Id](_.id)(a => b => b.copy(id = a))

  val existence: Lens[ProgramModel, Existence] =
    Lens[ProgramModel, Existence](_.existence)(a => b => b.copy(existence = a))

  val name: Lens[ProgramModel, Option[NonEmptyString]] =
    Lens[ProgramModel, Option[NonEmptyString]](_.name)(a => b => b.copy(name = a))

}
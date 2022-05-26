// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.data.StateT
import cats.syntax.all._
import cats.Eq
import clue.data.Input
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.refined._
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.lens._
import lucuma.odb.api.model.syntax.validatedinput._
import lucuma.core.model.{Program, Proposal}
import monocle.{Focus, Lens}

/**
 * A placeholder Program for now.
 */
final case class ProgramModel(
  id:        Program.Id,
  existence: Existence,
  name:      Option[NonEmptyString],
  proposal:  Option[Proposal]
)

object ProgramModel extends ProgramOptics {

  implicit val TopLevelProgram: TopLevelModel[Program.Id, ProgramModel] =
    TopLevelModel.instance(_.id, ProgramModel.existence)

  implicit val EqProgram: Eq[ProgramModel] =
    Eq.by(p => (p.id, p.existence, p.name, p.proposal))

  /**
   * Program creation input class.
   */
  final case class Create(
    programId: Option[Program.Id],
    name:      Option[NonEmptyString],
    proposal:  Option[ProposalInput]
  ) {

    val create: StateT[EitherInput, Database, ProgramModel] =
      for {
        x <- proposal.map(_.create).sequence.liftState
        i <- Database.program.getUnusedKey(programId)
        _ <- Database.program.saveNew(i, ProgramModel(i, Existence.Present, name, x))
        p <- Database.program.lookup(i)
      } yield p

  }

  object Create {

    implicit val DecoderCreate: Decoder[Create] =
      deriveDecoder[Create]

  }

  final case class Edit(
    programId: Program.Id,
    existence: Input[Existence]       = Input.ignore,
    name:      Input[NonEmptyString]  = Input.ignore,
    proposal:  Input[ProposalInput]   = Input.ignore
  ) {

    val edit: StateT[EitherInput, ProgramModel, Unit] = {
      for {
        ex <- existence.validateIsNotNull("existence").liftState
        _ <- ProgramModel.existence := ex
        _ <- ProgramModel.name      := name.toOptionOption
        _ <- ProgramModel.proposal  :? proposal
      } yield ()
    }
  }

  object Edit {

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderEdit: Decoder[Edit] =
      deriveConfiguredDecoder[Edit]

    implicit val EqEdit: Eq[Edit] =
      Eq.by { a => (
        a.programId,
        a.existence,
        a.name,
        a.proposal
      )}

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

  val id: Lens[ProgramModel, Program.Id] = Focus[ProgramModel](_.id)

  val existence: Lens[ProgramModel, Existence] = Focus[ProgramModel](_.existence)

  val name: Lens[ProgramModel, Option[NonEmptyString]] = Focus[ProgramModel](_.name)

  val proposal: Lens[ProgramModel, Option[Proposal]] = Focus[ProgramModel](_.proposal)

}

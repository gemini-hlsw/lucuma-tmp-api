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

  final case class PropertiesInput(
    name:      Input[NonEmptyString] = Input.ignore,
    proposal:  Input[ProposalInput]  = Input.ignore,
    existence: Input[Existence]      = Input.ignore
  ) {

    def create(
      programId: Program.Id
    ): ValidatedInput[ProgramModel] =
      proposal.toOption.traverse(_.create).map { p =>
        ProgramModel(
          programId,
          existence.toOption.getOrElse(Existence.Present),
          name.toOption,
          p
        )
      }

    def edit: StateT[EitherInput, ProgramModel, Unit] =
      for {
        e <- existence.validateIsNotNull("existence").liftState
        _ <- ProgramModel.existence := e
        _ <- ProgramModel.name      := name.toOptionOption
        _ <- ProgramModel.proposal  :? proposal
      } yield ()

  }

  object PropertiesInput {

    val Empty: PropertiesInput =
      PropertiesInput()

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderPropertiesInput: Decoder[PropertiesInput] =
      deriveConfiguredDecoder[PropertiesInput]

    implicit val EqPropertiesInput: Eq[PropertiesInput] =
      Eq.by { a => (
        a.name,
        a.proposal,
        a.existence
      )}

  }

  /**
   * Program creation input class.
   */
  final case class CreateInput(
    properties: Option[PropertiesInput]
  ) {

    val create: StateT[EitherInput, Database, ProgramModel] =
      for {
        i <- Database.program.cycleNextUnused
        r <- properties.getOrElse(PropertiesInput.Empty).create(i).liftState
        _ <- Database.program.saveNew(i, ProgramModel(i, r.existence, r.name, r.proposal))
        p <- Database.program.lookup(i)
      } yield p

  }

  object CreateInput {

    implicit val DecoderCreateInput: Decoder[CreateInput] =
      deriveDecoder[CreateInput]

    implicit val EqCreateInput: Eq[CreateInput] =
      Eq.by(_.properties)

  }

  final case class CreateResult(
    program: ProgramModel
  )

  object CreateResult {

    implicit val EqCreateResult: Eq[CreateResult] =
      Eq.by(_.program)

  }

  final case class SelectInput(
    programId: Option[Program.Id]
  ) {

    val go: StateT[EitherInput, Database, Option[ProgramModel]] =
      programId.traverse(Database.program.lookup)

  }

  object SelectInput {

    val Empty: SelectInput =
      SelectInput(None)

    def programId(pid: Program.Id): SelectInput =
      Empty.copy(programId = pid.some)

    implicit val DecoderSelectInput: Decoder[SelectInput] =
      deriveDecoder[SelectInput]

    implicit val EqSelectInput: Eq[SelectInput] =
      Eq.by(_.programId)

  }

  final case class EditInput(
    select: SelectInput,
    patch:  PropertiesInput
  ) {

    val editor: StateT[EitherInput, Database, Option[ProgramModel]] =
      for {
        p  <- select.go
        pʹ <- StateT.liftF[EitherInput, Database, Option[ProgramModel]](p.traverse(patch.edit.runS))
        _  <- pʹ.traverse(p => Database.program.update(p.id, p))
      } yield pʹ

  }

  object EditInput {

    implicit val DecoderEditInput: Decoder[EditInput] =
      deriveDecoder[EditInput]

    implicit val EqEdit: Eq[EditInput] =
      Eq.by { a => (
        a.select,
        a.patch
      )}

  }

  final case class EditResult(
    program: Option[ProgramModel]
  )

  object EditResult {

    implicit val EqEditResult: Eq[EditResult] =
      Eq.by(_.program)

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

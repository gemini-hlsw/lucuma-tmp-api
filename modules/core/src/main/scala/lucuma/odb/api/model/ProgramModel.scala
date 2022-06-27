// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.data.StateT
import cats.syntax.all._
import cats.Eq
import clue.data.Input
import eu.timepit.refined.cats._
import eu.timepit.refined.types.all.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.refined._
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.lens._
import lucuma.odb.api.model.syntax.validatedinput._
import lucuma.core.model.{Program, Proposal}
import lucuma.odb.api.model.query.SizeLimitedResult
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
    SET: Option[PropertiesInput]
  ) {

    val create: StateT[EitherInput, Database, ProgramModel] =
      for {
        i <- Database.program.cycleNextUnused
        r <- SET.getOrElse(PropertiesInput.Empty).create(i).liftState
        _ <- Database.program.saveNew(i, ProgramModel(i, r.existence, r.name, r.proposal))
        p <- Database.program.lookup(i)
      } yield p

  }

  object CreateInput {

    implicit val DecoderCreateInput: Decoder[CreateInput] =
      deriveDecoder[CreateInput]

    implicit val EqCreateInput: Eq[CreateInput] =
      Eq.by(_.SET)

  }

  final case class CreateResult(
    program: ProgramModel
  )

  object CreateResult {

    implicit val EqCreateResult: Eq[CreateResult] =
      Eq.by(_.program)

  }

  final case class UpdateInput(
    SET:   PropertiesInput,
    WHERE: Option[WhereProgramInput],
    LIMIT: Option[NonNegInt]
  ) {

    private def filteredPrograms(db: Database): List[ProgramModel] = {
      val ps = db.programs.rows.values
      WHERE.fold(ps)(where => ps.filter(where.matches)).toList
    }

    val editor: StateT[EitherInput, Database, SizeLimitedResult.Update[ProgramModel]] =
      for {
        ps  <- StateT.inspect[EitherInput, Database, List[ProgramModel]](filteredPrograms)
        psʹ <- StateT.liftF[EitherInput, Database, List[ProgramModel]](ps.traverse(SET.edit.runS))
        _   <- psʹ.traverse(p => Database.program.update(p.id, p))
      } yield SizeLimitedResult.Update.fromAll(psʹ, LIMIT)

  }

  object UpdateInput {

    implicit val DecoderUpdateInput: Decoder[UpdateInput] =
      deriveDecoder[UpdateInput]

    implicit val EqUpdateInput: Eq[UpdateInput] =
      Eq.by { a => (
        a.SET,
        a.WHERE,
        a.LIMIT
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

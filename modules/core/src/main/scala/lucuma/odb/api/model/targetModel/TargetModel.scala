// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.{Eq, Order}
import cats.data.{StateT, Validated}
import cats.syntax.apply._
import cats.syntax.option._
import clue.data.Input
import clue.data.syntax._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.refined._
import lucuma.core.model.{Program, SourceProfile, Target}
import lucuma.odb.api.model.{Database, EditorInput, EitherInput, Event, Existence, InputError, TopLevelModel, ValidatedInput}
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.lens._
import lucuma.odb.api.model.syntax.validatedinput._
import lucuma.odb.api.model.targetModel.SourceProfileModel.SourceProfileInput
import monocle.{Focus, Lens}


/**
 * TargetModel pairs an id with a `lucuma.core.model.Target`.
 */
final case class TargetModel(
  id:        Target.Id,
  existence: Existence,
  programId: Program.Id,
  target:    Target,
  observed:  Boolean
) {

  def name: NonEmptyString =
    target.name

  def clone(newId: Target.Id): TargetModel =
    copy(id = newId, existence = Existence.Present, observed = false)

}

object TargetModel extends TargetModelOptics {

  implicit val TopLevelTargetModel: TopLevelModel[Target.Id, TargetModel] =
    TopLevelModel.instance(_.id, TargetModel.existence)

  implicit val OrderTargetModel: Order[TargetModel] = {
    implicit val nameOrder: Order[Target] = Target.NameOrder

    Order.by { a =>
      (
        a.id,
        a.existence,
        a.programId,
        a.target,
        a.observed
      )
    }
  }

  final case class TargetInput(
    name:          Input[NonEmptyString]     = Input.ignore,
    sidereal:      Option[SiderealInput]     = None,
    nonsidereal:   Option[NonsiderealInput]  = None,
    sourceProfile: Input[SourceProfileInput] = Input.ignore
  ) extends EditorInput[Target] {

    override val create: ValidatedInput[Target] =
      (name.notMissing("name"),
       sourceProfile.notMissing("sourceProfile")
      ).tupled.andThen { case (n, sp) =>
        ValidatedInput.requireOne(
          "target",
          sidereal.map(_.createTarget(n, sp)),
          nonsidereal.map(_.createTarget(n, sp))
        )
      }

    override val edit: StateT[EitherInput, Target, Unit] = {
      val validArgs =
        (name.validateIsNotNull("name"),
         sourceProfile.validateIsNotNull("sourceProfile"),
         Validated.condNec(
           sidereal.isEmpty || nonsidereal.isEmpty,
           (),
           InputError.fromMessage("Edit `sidereal` or `nonsidereal` but not both.")
         )
        ).tupled

      for {
        args <- validArgs.liftState
        (n, sp, _) = args
        _ <- Target.name          := n
        _ <- Target.sourceProfile :< sp.map(_.edit)
        _ <- sidereal.map(_.targetEditor).orElse(nonsidereal.map(_.targetEditor)).getOrElse(
          StateT.empty[EitherInput, Target, Unit]
        )
      } yield ()
    }
  }

  object TargetInput {

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderInput: Decoder[TargetInput] =
      deriveConfiguredDecoder[TargetInput]

    implicit val EqTargetInput: Eq[TargetInput] =
      Eq.by { a => (
        a.name,
        a.sidereal,
        a.nonsidereal,
        a.sourceProfile
      )}

  }

  final case class Create(
    programId: Program.Id,
    create:    TargetInput
  ) {

    val createTarget: StateT[EitherInput, Database, TargetModel] =

      for {
        i <- Database.target.cycleNextUnused
        _ <- Database.program.lookup(programId)
        t  = create.create.map(TargetModel(i, Existence.Present, programId, _, observed = false))
        _ <- Database.target.saveNewIfValid(t)(_.id)
        r <- Database.target.lookup(i)
      } yield r

  }

  object Create {

    def sidereal(
      programId:     Program.Id,
      name:          NonEmptyString,
      input:         SiderealInput,
      sourceProfile: SourceProfileInput
    ): Create =
      Create(programId, TargetInput(name.assign, input.some, None, sourceProfile.assign))

    def nonsidereal(
      programId:     Program.Id,
      name:          NonEmptyString,
      input:         NonsiderealInput,
      sourceProfile: SourceProfileInput
    ): Create =
      Create(programId, TargetInput(name.assign, None, input.some, sourceProfile.assign))

    implicit val DecoderCreate: Decoder[Create] =
      deriveDecoder[Create]

    implicit val EqCreate: Eq[Create] =
      Eq.by { a => (
        a.programId,
        a.create
      )}
  }

  final case class Edit(
    targetId:  Target.Id,
    edit:      Input[TargetInput] = Input.ignore,
    existence: Input[Existence]   = Input.ignore
  ) {

    val editor: StateT[EitherInput, TargetModel, Unit] = {

      val validArgs =
        (existence.validateIsNotNull("existence"),
         edit.validateIsNotNull("edit")
        ).tupled

      for {
        args <- validArgs.liftState
        (e, _) = args
        _ <- TargetModel.existence := e
        _ <- TargetModel.target    :< edit.toOption.map(_.edit)
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
        a.targetId,
        a.edit,
        a.existence
      )}

  }

  final case class TargetEvent (
    id:       Long,
    editType: Event.EditType,
    value:    TargetModel,
  ) extends Event.Edit[TargetModel]

  object TargetEvent {

    def created(value: TargetModel)(id: Long): TargetEvent =
      TargetEvent(id, Event.EditType.Created, value)

    def updated(value: TargetModel)(id: Long): TargetEvent =
      TargetEvent(id, Event.EditType.Updated, value)

  }

}

trait TargetModelOptics { self: TargetModel.type =>

  val id: Lens[TargetModel, Target.Id] =
    Focus[TargetModel](_.id)

  val existence: Lens[TargetModel, Existence] =
    Focus[TargetModel](_.existence)

  val target: Lens[TargetModel, Target] =
    Focus[TargetModel](_.target)

  val name: Lens[TargetModel, NonEmptyString] =
    target.andThen(Target.name)

  val sourceProfile: Lens[TargetModel, SourceProfile] =
    target.andThen(Target.sourceProfile)

  val observed: Lens[TargetModel, Boolean] =
    Focus[TargetModel](_.observed)

}

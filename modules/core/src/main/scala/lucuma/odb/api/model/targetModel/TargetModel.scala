// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.{Eq, Order}
import cats.data.StateT
import cats.syntax.apply._
import cats.syntax.option._
import clue.data.Input
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.refined._
import lucuma.core.model.{Program, SourceProfile, Target}
import lucuma.odb.api.model.{Database, EitherInput, Event, Existence, TopLevelModel, ValidatedInput}
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

  final case class Create(
    targetId:      Option[Target.Id],
    name:          NonEmptyString,
    sidereal:      Option[SiderealInput],
    nonsidereal:   Option[NonsiderealInput],
    sourceProfile: SourceProfileInput
  ) {

    def create2(
      programId: Program.Id
    ): StateT[EitherInput, Database, TargetModel] =

      for {
        i <- Database.target.getUnusedKey(targetId)
        _ <- Database.program.lookup(programId)
        t  = ValidatedInput.requireOne(
          "target",
          sidereal.map(_.createTarget(name, sourceProfile)),
          nonsidereal.map(_.createTarget(name, sourceProfile))
        )
        tm = t.map(TargetModel(i, Existence.Present, programId, _, observed = false))
        _ <- Database.target.saveNewIfValid(tm)(_.id)
        r <- Database.target.lookup(i)
      } yield r

  }

  object Create {

    def sidereal(
      targetId:      Option[Target.Id],
      name:          NonEmptyString,
      input:         SiderealInput,
      sourceProfile: SourceProfileInput
    ): Create =
      Create(targetId, name, input.some, None, sourceProfile)

    def nonsidereal(
      targetId:      Option[Target.Id],
      name:          NonEmptyString,
      input:         NonsiderealInput,
      sourceProfile: SourceProfileInput
    ): Create =
      Create(targetId, name, None, input.some, sourceProfile)

    implicit val DecoderCreate: Decoder[Create] =
      deriveDecoder[Create]

    implicit val EqCreate: Eq[Create] =
      Eq.by { a => (
        a.targetId,
        a.name,
        a.sidereal,
        a.nonsidereal,
        a.sourceProfile
      )}
  }

  final case class Edit(
    targetId:      Target.Id,
    existence:     Input[Existence]          = Input.ignore,
    name:          Input[NonEmptyString]     = Input.ignore,
    sidereal:      Option[SiderealInput]     = None,
    nonsidereal:   Option[NonsiderealInput]  = None,
    sourceProfile: Input[SourceProfileInput] = Input.ignore
  ) {

    val editor: StateT[EitherInput, TargetModel, Unit] = {

      val validArgs =
        (existence    .validateIsNotNull("existence"),
         name         .validateIsNotNull("name"),
         sourceProfile.validateIsNotNull("sourceProfile")
        ).tupled

      for {
        args <- validArgs.liftState
        (e, n, p) = args
        _ <- TargetModel.existence     := e
        _ <- TargetModel.name          := n
        _ <- TargetModel.target        :< sidereal.map(_.targetEditor)
        _ <- TargetModel.target        :< nonsidereal.map(_.targetEditor)
        _ <- TargetModel.sourceProfile :< p.map(_.edit)
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
        a.existence,
        a.name,
        a.sidereal,
        a.nonsidereal
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

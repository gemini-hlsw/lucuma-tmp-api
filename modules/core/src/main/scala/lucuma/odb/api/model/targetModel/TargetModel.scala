// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.{Eq, Order}
import cats.data.{StateT, Validated}
import cats.syntax.apply._
import cats.syntax.option._
import cats.syntax.traverse._
import clue.data.Input
import clue.data.syntax._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.types.numeric.NonNegInt
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.refined._
import lucuma.core.model.{Observation, Program, SourceProfile, Target}
import lucuma.odb.api.model.{Database, EitherInput, Event, Existence, InputError, TopLevelModel, ValidatedInput, WhereObservationInput}
import lucuma.odb.api.model.query.SizeLimitedResult
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
    copy(
      id        = newId,
      existence = Existence.Present,
      observed  = false
    )

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

  final case class PropertiesInput(
    name:          Input[NonEmptyString]     = Input.ignore,
    sidereal:      Option[SiderealInput]     = None,
    nonsidereal:   Option[NonsiderealInput]  = None,
    sourceProfile: Input[SourceProfileInput] = Input.ignore,
    existence:     Input[Existence]          = Input.ignore
  ) {

    val createTarget: ValidatedInput[Target] =
      (name.notMissing("name"),
       sourceProfile.notMissing("sourceProfile")
      ).tupled.andThen { case (n, sp) =>
        ValidatedInput.requireOne(
          "target",
          sidereal.map(_.createTarget(n, sp)),
          nonsidereal.map(_.createTarget(n, sp))
        )
      }

    def create(id: Target.Id, pid: Program.Id): ValidatedInput[TargetModel] =
      createTarget.map { t =>
        TargetModel(id, existence.toOption.getOrElse(Existence.Present), pid, t, observed = false)
      }

    val editTarget: StateT[EitherInput, Target, Unit] = {
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

    val edit: StateT[EitherInput, TargetModel, Unit] =
      for {
        e <- existence.validateIsNotNull("existence").liftState[TargetModel]
        _ <- TargetModel.existence := e
        _ <- TargetModel.target.transform(editTarget)
      } yield ()
  }

  object PropertiesInput {

    val Empty: PropertiesInput =
      PropertiesInput()

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderInput: Decoder[PropertiesInput] =
      deriveConfiguredDecoder[PropertiesInput]

    implicit val EqTargetInput: Eq[PropertiesInput] =
      Eq.by { a => (
        a.name,
        a.sidereal,
        a.nonsidereal,
        a.sourceProfile,
        a.existence
      )}

  }

  final case class CreateInput(
    programId: Program.Id,
    SET:       Option[PropertiesInput]
  ) {

    val createTarget: StateT[EitherInput, Database, TargetModel] =

      for {
        i <- Database.target.cycleNextUnused
        _ <- Database.program.lookup(programId)
        t  = SET.getOrElse(PropertiesInput.Empty).create(i, programId)
        _ <- Database.target.saveNewIfValid(t)(_.id)
        r <- Database.target.lookup(i)
      } yield r

  }

  object CreateInput {

    def sidereal(
      programId:     Program.Id,
      name:          NonEmptyString,
      input:         SiderealInput,
      sourceProfile: SourceProfileInput
    ): CreateInput =
      CreateInput(programId, PropertiesInput(name.assign, input.some, None, sourceProfile.assign).some)

    def nonsidereal(
      programId:     Program.Id,
      name:          NonEmptyString,
      input:         NonsiderealInput,
      sourceProfile: SourceProfileInput
    ): CreateInput =
      CreateInput(programId, PropertiesInput(name.assign, None, input.some, sourceProfile.assign).some)

    implicit val DecoderCreate: Decoder[CreateInput] =
      deriveDecoder[CreateInput]

    implicit val EqCreate: Eq[CreateInput] =
      Eq.by { a => (
        a.programId,
        a.SET
      )}
  }

  final case class CreateResult(
    target: TargetModel
  )

  object CreateResult {

    implicit val EqCreateResult: Eq[CreateResult] =
      Eq.by(_.target)

  }

  final case class UpdateInput(
    SET:   PropertiesInput,
    WHERE: Option[WhereTargetInput],
    LIMIT: Option[NonNegInt]
  ) {

    private def filteredTargets(db: Database): List[TargetModel] = {
      val ts = db.targets.rows.values
      WHERE.fold(ts)(where => ts.filter(where.matches)).toList
    }


    val editor: StateT[EitherInput, Database, SizeLimitedResult.Update[TargetModel]] =
      for {
        ts  <- StateT.inspect[EitherInput, Database, List[TargetModel]](filteredTargets)
        tsʹ <- StateT.liftF[EitherInput, Database, List[TargetModel]](ts.traverse(SET.edit.runS))
        _   <- tsʹ.traverse(t => Database.target.update(t.id, t))
      } yield SizeLimitedResult.Update.fromAll(tsʹ, LIMIT)

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

  final case class CloneResult(
    originalTarget: TargetModel,
    newTarget:      TargetModel
  )

  object CloneResult {

    implicit val EqCloneResult: Eq[CloneResult] =
      Eq.by { a => (
        a.originalTarget,
        a.newTarget
      )}

  }

  final case class CloneInput(
    targetId:   Target.Id,
    SET:        Option[PropertiesInput],
    REPLACE_IN: Option[List[Observation.Id]]
  ) {

    val go: StateT[EitherInput, Database, CloneResult] =
      for {
        t  <- Database.target.lookup(targetId)
        i  <- Database.target.cycleNextUnused
        c   = t.clone(i)
        _  <- Database.target.saveNew(i, c)
        cʹ <- SET.fold(StateT.pure[EitherInput, Database, TargetModel](c)) { p =>
          TargetModel.UpdateInput(
            p,
            WhereTargetInput.MatchAll.withId(i).some,
            None
          ).editor.map(_.limitedValues.head)
        }
      } yield CloneResult(originalTarget = t, newTarget = cʹ)

    def replaceWhereObservation: Option[WhereObservationInput] =
      REPLACE_IN.map(WhereObservationInput.MatchAll.withIds)

  }

  object CloneInput {

    implicit val DecoderCloneInput: Decoder[CloneInput] =
      deriveDecoder[CloneInput]

    implicit val EqCloneInput: Eq[CloneInput] =
      Eq.by { a => (
        a.targetId,
        a.SET,
        a.REPLACE_IN
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

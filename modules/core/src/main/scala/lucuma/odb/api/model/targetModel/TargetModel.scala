// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.{Eq, Monad, Order}
import cats.data.{EitherNec, StateT}
import cats.mtl.Stateful
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.traverse._
import clue.data.{Assign, Ignore, Input, Unassign}
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.refined._
import io.circe.generic.semiauto._
import lucuma.core.model.{AngularSize, Program, Target}
import lucuma.odb.api.model.{DatabaseState, Event, Existence, InputError, TopLevelModel, ValidatedInput}
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.lens._
import lucuma.odb.api.model.targetModel.SourceProfileModel.CreateSourceProfileInput
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
    sourceProfile: CreateSourceProfileInput,
    sidereal:      Option[CreateSiderealInput],
    nonsidereal:   Option[CreateNonsiderealInput],
    angularSize:   Option[AngularSizeInput]
  ) {

    def create[F[_]: Monad, T](
      programId: Program.Id,
      db:        DatabaseState[T]
    )(implicit S: Stateful[F, T]): F[ValidatedInput[TargetModel]] =

      for {
        i  <- db.target.getUnusedId(targetId)
        p  <- db.program.lookupValidated(programId)
        sp <- S.monad.pure(sourceProfile.toSourceProfile)
        a  <- S.monad.pure(angularSize.traverse(_.create))
        t  = ValidatedInput.requireOne("target",
          sidereal.map(_.toGemTarget(name, sp, a)),
          nonsidereal.map(_.toGemTarget(name, sp, a))
        )
        tm = (i, p, t).mapN { (iʹ, _, tʹ) =>
          TargetModel(iʹ, Existence.Present, programId, tʹ, observed = false)
        }
        _ <- db.target.saveNewIfValid(tm)(_.id)
      } yield tm

  }

  object Create {

    def sidereal(
      targetId:      Option[Target.Id],
      name:          NonEmptyString,
      sourceProfile: CreateSourceProfileInput,
      input:         CreateSiderealInput
    ): Create =
      Create(targetId, name, sourceProfile, input.some, None, None)

    def nonsidereal(
      targetId:      Option[Target.Id],
      name:          NonEmptyString,
      sourceProfile: CreateSourceProfileInput,
      input:         CreateNonsiderealInput
    ): Create =
      Create(targetId, name, sourceProfile, None, input.some, None)

    implicit val DecoderCreate: Decoder[Create] =
      deriveDecoder[Create]

    implicit val EqCreate: Eq[Create] =
      Eq.by { a => (
        a.targetId,
        a.name,
        a.sourceProfile,
        a.sidereal,
        a.nonsidereal,
        a.angularSize
      )}
  }

  final case class Edit(
    targetId:    Target.Id,
    existence:   Input[Existence]             = Input.ignore,
    name:        Input[NonEmptyString]        = Input.ignore,
    sidereal:    Option[EditSiderealInput]    = None,
    nonSidereal: Option[EditNonsiderealInput] = None,
    angularSize: Input[AngularSizeInput]      = Input.ignore
  ) {

    val editor: StateT[EitherNec[InputError, *], TargetModel, Unit] = {
      val validArgs =
        (
          existence.validateIsNotNull("existence"),
          name     .validateIsNotNull("name")
        ).tupled.toEither

      def editTarget(s: Option[StateT[EitherNec[InputError, *], Target, Unit]]): StateT[EitherNec[InputError, *], TargetModel, Unit] =
        s.getOrElse(StateT.empty[EitherNec[InputError, *], Target, Unit])
         .transformS(_.target, (m, t) => TargetModel.target.replace(t)(m))

      val stateAngularSize: StateT[EitherNec[InputError, *], Target, Unit] =
        angularSize match {
          // Do nothing
          case Ignore         =>
            StateT.empty[EitherNec[InputError, *], Target, Unit]

          // Remove angular size
          case Unassign       =>
            (Target.angularSize := Option.empty[AngularSize]).void

          // Create or edit, depending on whether it already exists
          case Assign(editor) =>
            StateT.modifyF[EitherNec[InputError, *], Target] { t =>
              Target
                .angularSize
                .modifyF[EitherNec[InputError, *]](
                  _.fold(editor.create.toEither)(as => editor.edit.runS(as)).map(_.some)
                )(t)
            }
        }

      for {
        args <- StateT.liftF(validArgs)
        (e, n) = args
        _ <- TargetModel.existence := e
        _ <- TargetModel.name      := n
        _ <- editTarget(sidereal.map(_.editor))
        _ <- editTarget(nonSidereal.map(_.editor))
        _ <- stateAngularSize.transformS[TargetModel](_.target, (m, t) => TargetModel.target.replace(t)(m))
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
        a.nonSidereal
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

  val observed: Lens[TargetModel, Boolean] =
    Focus[TargetModel](_.observed)

}

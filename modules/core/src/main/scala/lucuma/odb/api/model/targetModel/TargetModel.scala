// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.{Eq, Monad, Order}
import cats.data.State
import cats.mtl.Stateful
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import clue.data.Input
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.refined._
import io.circe.generic.semiauto._
import lucuma.core.model.{Program, Target}
import lucuma.core.optics.state.all._
import lucuma.core.optics.syntax.lens._
import lucuma.odb.api.model.{DatabaseState, Event, Existence, TopLevelModel, ValidatedInput}
import lucuma.odb.api.model.syntax.input._
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

}

object TargetModel extends TargetModelOptics {

  implicit val TopLevelTargetModel: TopLevelModel[Target.Id, TargetModel] =
    TopLevelModel.instance(_.id, TargetModel.existence)

  implicit val OrderTargetModel: Order[TargetModel] = {
    implicit val nameOrder: Order[Target] = Target.NameOrder

    Order.by { a =>
      (
        a.existence,
        a.target,
        a.id,
        a.observed
      )
    }
  }

  final case class Create(
    targetId:    Option[Target.Id],
    programId:   Program.Id,
    sidereal:    Option[CreateSiderealInput],
    nonsidereal: Option[CreateNonsiderealInput]
  ) {

    def create[F[_]: Monad, T](
      db: DatabaseState[T]
    )(implicit S: Stateful[F, T]): F[ValidatedInput[TargetModel]] =

      for {
        i <- db.target.getUnusedId(targetId)
        p <- db.program.lookupValidated(programId)
        t  = ValidatedInput.requireOne("target",
          sidereal.map(_.toGemTarget),
          nonsidereal.map(_.toGemTarget)
        )
        tm = (i, p, t).mapN { (iʹ, _, tʹ) =>
          TargetModel(iʹ, Existence.Present, programId, tʹ, observed = false)
        }
        _ <- db.target.saveNewIfValid(tm)(_.id)
      } yield tm

  }

  object Create {

    implicit val DecoderCreate: Decoder[Create] =
      deriveDecoder[Create]

    implicit val EqCreate: Eq[Create] =
      Eq.by { a => (
        a.targetId,
        a.programId,
        a.sidereal,
        a.nonsidereal
      )}
  }

  final case class Edit(
    targetId:    Target.Id,
    existence:   Input[Existence]             = Input.ignore,
    name:        Input[NonEmptyString]        = Input.ignore,
    sidereal:    Option[EditSiderealInput]    = None,
    nonSidereal: Option[EditNonsiderealInput] = None
  ) {

    def edit(t: TargetModel): ValidatedInput[TargetModel] =
      editor.map(_.runS(t).value)

    private val editor: ValidatedInput[State[TargetModel, Unit]] =
      (existence.validateIsNotNull("existence"),
       name     .validateIsNotNull("name"),
       sidereal.traverse(_.editor),
       nonSidereal.traverse(_.editor)
      ).mapN { (e, n, s, ns) =>
        for {
          _ <- TargetModel.existence := e
          _ <- TargetModel.name      := n
          _ <- s.fold(State.get[TargetModel].void) { ed =>
            TargetModel.target.mod_(ed.runS(_).value)
          }
          _ <- ns.fold(State.get[TargetModel].void) { ed =>
            TargetModel.target.mod_(ed.runS(_).value)
          }
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

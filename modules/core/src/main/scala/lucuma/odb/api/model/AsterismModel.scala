// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.Existence._
import lucuma.odb.api.model.syntax.input._
import lucuma.core.model.{Asterism, Program}
import lucuma.core.math.Coordinates
import lucuma.core.optics.syntax.lens._
import cats.Eq
import cats.data.State
import cats.implicits._
import clue.data.Input
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string._
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.refined._
import monocle.Lens

final case class AsterismModel(
  id:           Asterism.Id,
  existence:    Existence,
  name:         Option[NonEmptyString],
  explicitBase: Option[Coordinates]
)

object AsterismModel extends AsterismOptics {

  sealed trait Type extends Product with Serializable

  implicit val TopLevelAsterism: TopLevelModel[Asterism.Id, AsterismModel] =
    TopLevelModel.instance(_.id, AsterismModel.existence)


  implicit val EqAsterism: Eq[AsterismModel] =
    Eq.by(d => (d.id, d.existence, d.name, d.explicitBase))


  final case class Create(
    asterismId:   Option[Asterism.Id],
    name:         Option[NonEmptyString],
    programIds:   List[Program.Id],
    explicitBase: Option[CoordinatesModel.Input]
  ) {

    def withId: ValidatedInput[Asterism.Id => AsterismModel] =
      explicitBase
        .traverse(_.toCoordinates)
        .map(b => aid => AsterismModel(aid, Present, name, b))

  }

  object Create {

    implicit val DecoderCreate: Decoder[Create] =
      deriveDecoder[Create]

    implicit val EqCreate: Eq[Create] =
      Eq.by { a => (
        a.asterismId,
        a.name,
        a.programIds,
        a.explicitBase,
      )}

  }

  final case class Edit(
    asterismId:   Asterism.Id,
    existence:    Input[Existence]              = Input.ignore,
    name:         Input[NonEmptyString]         = Input.ignore,
    explicitBase: Input[CoordinatesModel.Input] = Input.ignore
  ) {

    def id: Asterism.Id =
      asterismId

    def editor: ValidatedInput[State[AsterismModel, Unit]] = {
      (existence   .validateIsNotNull("existence"),
       explicitBase.validateNullable(_.toCoordinates)
      ).mapN { (e, b) =>
        for {
          _ <- AsterismModel.existence    := e
          _ <- AsterismModel.name         := name.toOptionOption
          _ <- AsterismModel.explicitBase := b
        } yield ()
      }
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
        a.asterismId,
        a.existence,
        a.name,
        a.explicitBase
      )}

  }

  final case class AsterismEvent (
    id:       Long,
    editType: Event.EditType,
    value:    AsterismModel
  ) extends Event.Edit[AsterismModel]

  object AsterismEvent {
    def created(value: AsterismModel)(id: Long): AsterismEvent =
      AsterismEvent(id, Event.EditType.Created, value)

    def updated(value: AsterismModel)(id: Long): AsterismEvent =
      AsterismEvent(id, Event.EditType.Updated, value)
  }

}

trait AsterismOptics { self: AsterismModel.type =>

  val id: Lens[AsterismModel, Asterism.Id] =
    Lens[AsterismModel, Asterism.Id](_.id)(a => _.copy(id = a))

  val existence: Lens[AsterismModel, Existence] =
    Lens[AsterismModel, Existence](_.existence)(a => _.copy(existence = a))

  val name: Lens[AsterismModel, Option[NonEmptyString]] =
    Lens[AsterismModel, Option[NonEmptyString]](_.name)(a => _.copy(name = a))

  val explicitBase: Lens[AsterismModel, Option[Coordinates]] =
    Lens[AsterismModel, Option[Coordinates]](_.explicitBase)(a => _.copy(explicitBase = a))

}

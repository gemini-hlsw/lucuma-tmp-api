// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.Existence._
import lucuma.core.model.{Asterism, Program, Target}
import lucuma.core.util.Enumerated
import lucuma.core.math.Coordinates
import lucuma.core.optics.syntax.lens._
import cats.Eq
import cats.data.{Nested, State}
import cats.implicits._
import io.circe.Decoder
import io.circe.generic.semiauto._
import monocle.{Lens, Optional, Prism}

sealed trait AsterismModel {

  def id:           Asterism.Id
  def existence:    Existence

  def explicitBase: Option[Coordinates]
  def tpe:          AsterismModel.Type
  def targetIds:    Set[Target.Id]

  def fold[B](
    default: AsterismModel.Default => B,
    ghost:   AsterismModel.Ghost   => B
  ): B =
    this match {
      case a: AsterismModel.Default => default(a)
      case a: AsterismModel.Ghost   => ghost(a)
    }

}

object AsterismModel extends AsterismOptics {

  sealed trait Type extends Product with Serializable

  object Type {
    case object Default extends Type
    case object Ghost   extends Type  // just to have a second one...

    implicit val EnumeratedType: Enumerated[Type] =
      Enumerated.of(Default, Ghost)
  }

  implicit val TopLevelAsterism: TopLevelModel[Asterism.Id, AsterismModel] =
    TopLevelModel.instance(_.id, AsterismModel.existence)

  final case class Default(
    id:           Asterism.Id,
    existence:    Existence,
    explicitBase: Option[Coordinates],
    targetIds:    Set[Target.Id]
  ) extends AsterismModel {

    def tpe: Type =
      Type.Default

  }

  object Default extends DefaultOptics {

    implicit val EqDefault: Eq[Default] =
      Eq.by(d => (d.id, d.existence, d.explicitBase, d.targetIds))

  }

  trait DefaultOptics { self: Default.type =>

    val select: Prism[AsterismModel, Default] =
      Prism.partial[AsterismModel, Default]{case d: Default => d}(identity)

    val id: Lens[Default, Asterism.Id] =
      Lens[Default, Asterism.Id](_.id)(a => b => b.copy(id = a))

    val asterismId: Optional[AsterismModel, Asterism.Id] =
      select ^|-> id

    val existence: Lens[Default, Existence] =
      Lens[Default, Existence](_.existence)(a => b => b.copy(existence = a))

    val asterismExistence: Optional[AsterismModel, Existence] =
      select ^|-> existence

    val explicitBase: Lens[Default, Option[Coordinates]] =
      Lens[Default, Option[Coordinates]](_.explicitBase)(a => b => b.copy(explicitBase = a))

    val asterismExplicitBase: Optional[AsterismModel, Option[Coordinates]] =
      select ^|-> explicitBase

    val targetIds: Lens[Default, Set[Target.Id]] =
      Lens[Default, Set[Target.Id]](_.targetIds)(a => b => b.copy(targetIds = a))

    val asterismTargetIds: Optional[AsterismModel, Set[Target.Id]] =
      select ^|-> targetIds

  }

  trait Create[T] {
    def asterismId: Option[Asterism.Id]
    def programIds: List[Program.Id]  // to share immediately with the indicated programs
    def targetIds:  Set[Target.Id]
    def withId:     ValidatedInput[Asterism.Id => T]
  }

  final case class CreateDefault(
    asterismId:   Option[Asterism.Id],
    programIds:   List[Program.Id],
    explicitBase: Option[CoordinatesModel.Input],
    targetIds:    Set[Target.Id]
  ) extends Create[AsterismModel.Default] {

    override def withId: ValidatedInput[Asterism.Id => AsterismModel.Default] =
      explicitBase.traverse(_.toCoordinates).map(c => aid => Default(aid, Present, c, targetIds))

  }

  object CreateDefault {

    implicit val DecoderCreateDefault: Decoder[CreateDefault] =
      deriveDecoder[CreateDefault]

  }

  // not meant to be realistic yet
  final case class Ghost(
    id:           Asterism.Id,
    existence:    Existence,
    explicitBase: Option[Coordinates],
    ifu1:         Target.Id,
    ifu2:         Option[Target.Id]
  ) extends AsterismModel {

    def tpe: Type =
      Type.Ghost

    def targetIds: Set[Target.Id] =
      ifu2.foldLeft(Set(ifu1))(_ + _)

  }

  object Ghost {

    implicit val EqGhost: Eq[Ghost] =
      Eq.by(g => (g.id, g.existence, g.explicitBase, g.ifu1, g.ifu2))

  }

  final case class EditDefault(
    asterismId:   Asterism.Id,
    existence:    Option[Existence],
    explicitBase: Option[Option[CoordinatesModel.Input]],
    targetIds:    Option[Set[Target.Id]]
  ) extends Editor[Asterism.Id, AsterismModel.Default] {

    override def id: Asterism.Id =
      asterismId

    override def editor: ValidatedInput[State[AsterismModel.Default, Unit]] =
      Nested(explicitBase).traverse(_.toCoordinates).map { b =>
        for {
          _ <- Default.existence    := existence
          _ <- Default.explicitBase := b.value
          _ <- Default.targetIds    := targetIds
        } yield ()
      }
  }

  object EditDefault {

    implicit val DecoderEditDefault: Decoder[EditDefault] =
      deriveDecoder[EditDefault]

  }

  final case class AsterismProgramLinks(
    asterismId: Asterism.Id,
    programIds: List[Program.Id]
  )

  object AsterismProgramLinks {

    implicit val DecoderAsterismProgramLinks: Decoder[AsterismProgramLinks] =
      deriveDecoder[AsterismProgramLinks]

  }

  implicit val EqAsterism: Eq[AsterismModel] =
    Eq.instance[AsterismModel] {
      case (a: Default, b: Default) => a === b
      case (a: Ghost, b: Ghost)     => a === b
      case (_, _)                   => false
    }

  final case class AsterismEvent (
    id:       Long,
    editType: Event.EditType,
    value:    AsterismModel
  ) extends Event.Edit[AsterismModel]

  object AsterismEvent {
    def apply(editType: Event.EditType, value: AsterismModel)(id: Long): AsterismEvent =
      AsterismEvent(id, editType, value)
  }

}

trait AsterismOptics { self: AsterismModel.type =>

  val existence: Lens[AsterismModel, Existence] =
    Lens[AsterismModel, Existence](_.existence) { a =>
      _.fold(
        _.copy(existence = a),
        _.copy(existence = a)
      )
    }

}

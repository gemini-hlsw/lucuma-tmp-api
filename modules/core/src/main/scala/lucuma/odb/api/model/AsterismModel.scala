// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.Existence._
import lucuma.odb.api.model.syntax.all._
import lucuma.odb.api.model.json.targetmath._
import lucuma.core.util.{Enumerated, Gid}
import lucuma.core.math.Coordinates

import cats.Eq
import cats.data.State
import cats.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.PosLong
import io.circe.Decoder
import io.circe.generic.semiauto._
import monocle.{Lens, Optional, Prism}

sealed trait AsterismModel {

  def id:           AsterismModel.Id
  def existence:    Existence

  def explicitBase: Option[Coordinates]
  def tpe:          AsterismModel.Type
  def targets:      List[TargetModel.Id]

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

  final case class Id(value: PosLong) {
    override def toString: String =
      Gid[Id].show(this)
  }

  object Id {
    implicit val GidAsterismId: Gid[Id] =
      Gid.instance('a', _.value, apply)
  }

  implicit val TopLevelAsterism: TopLevelModel[Id, AsterismModel] =
    TopLevelModel.instance(_.id, AsterismModel.existence)

  final case class Default(
    id:           AsterismModel.Id,
    existence:    Existence,
    explicitBase: Option[Coordinates],
    targets:      List[TargetModel.Id]
  ) extends AsterismModel {

    def tpe: Type =
      Type.Default

  }

  object Default extends DefaultOptics {

    implicit val EqDefault: Eq[Default] =
      Eq.by(d => (d.id, d.existence, d.explicitBase, d.targets))

  }

  trait DefaultOptics { self: Default.type =>

    val select: Prism[AsterismModel, Default] =
      Prism.partial[AsterismModel, Default]{case d: Default => d}(identity)

    val id: Lens[Default, AsterismModel.Id] =
      Lens[Default, AsterismModel.Id](_.id)(a => b => b.copy(id = a))

    val asterismId: Optional[AsterismModel, AsterismModel.Id] =
      select ^|-> id

    val existence: Lens[Default, Existence] =
      Lens[Default, Existence](_.existence)(a => b => b.copy(existence = a))

    val asterismExistence: Optional[AsterismModel, Existence] =
      select ^|-> existence

    val explicitBase: Lens[Default, Option[Coordinates]] =
      Lens[Default, Option[Coordinates]](_.explicitBase)(a => b => b.copy(explicitBase = a))

    val asterismExplicitBase: Optional[AsterismModel, Option[Coordinates]] =
      select ^|-> explicitBase

    val targets: Lens[Default, List[TargetModel.Id]] =
      Lens[Default, List[TargetModel.Id]](_.targets)(a => b => b.copy(targets = a))

    val asterismTargets: Optional[AsterismModel, List[TargetModel.Id]] =
      select ^|-> targets

  }

  trait Create {
    def programs: List[ProgramModel.Id]  // to share immediately with the indicated programs
    def targets:  List[TargetModel.Id]
    def withId(aid: AsterismModel.Id): AsterismModel
  }

  final case class CreateDefault(
    programs:     List[ProgramModel.Id],
    explicitBase: Option[Coordinates],
    targets:      List[TargetModel.Id]
  ) extends Create {

    override def withId(aid: AsterismModel.Id): AsterismModel =
      Default(aid, Present, explicitBase, targets)

  }

  object CreateDefault {

    implicit val DecoderCreateDefault: Decoder[CreateDefault] =
      deriveDecoder[CreateDefault]

  }

  // not meant to be realistic yet
  final case class Ghost(
    id:           AsterismModel.Id,
    existence:    Existence,
    explicitBase: Option[Coordinates],
    ifu1:         TargetModel.Id,
    ifu2:         Option[TargetModel.Id]
  ) extends AsterismModel {

    def tpe: Type =
      Type.Ghost

    def targets: List[TargetModel.Id] =
      ifu1 :: ifu2.toList

  }

  object Ghost {

    implicit val EqGhost: Eq[Ghost] =
      Eq.by(g => (g.id, g.existence, g.explicitBase, g.ifu1, g.ifu2))

  }

  final case class EditDefault(
    id:           AsterismModel.Id,
    existence:    Option[Existence],
    explicitBase: Option[Option[Coordinates]],
    targets:      Option[List[TargetModel.Id]]
  ) extends Editor[Id, AsterismModel] {

    override def editor: State[AsterismModel, Unit] =
      for {
        _ <- Default.asterismExistence    := existence
        _ <- Default.asterismExplicitBase := explicitBase
        _ <- Default.asterismTargets      := targets
      } yield ()
  }

  object EditDefault {

    implicit val DecoderEditDefault: Decoder[EditDefault] =
      deriveDecoder[EditDefault]

  }

  implicit val EqAsterism: Eq[AsterismModel] =
    Eq.instance[AsterismModel] {
      case (a: Default, b: Default) => a === b
      case (a: Ghost, b: Ghost)     => a === b
      case (_, _)                   => false
    }

  final case class AsterismCreatedEvent (
    id:    Long,
    value: AsterismModel,
  ) extends Event.Created[AsterismModel]

  object AsterismCreatedEvent {
    def apply(value: AsterismModel)(id: Long): AsterismCreatedEvent =
      AsterismCreatedEvent(id, value)
  }

  final case class AsterismEditedEvent (
    id:       Long,
    oldValue: AsterismModel,
    newValue: AsterismModel
  ) extends Event.Edited[AsterismModel]

  object AsterismEditedEvent {
    def apply(oldValue: AsterismModel, newValue: AsterismModel)(id: Long): AsterismEditedEvent =
      AsterismEditedEvent(id, oldValue, newValue)
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

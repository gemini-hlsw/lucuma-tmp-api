// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.Existence._
import lucuma.odb.api.model.syntax.all._
import lucuma.core.util.{Enumerated, Gid}
import lucuma.core.math.Coordinates
import cats.Eq
import cats.data.{Nested, State}
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
  def targetIds:    Set[TargetModel.Id]

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
    targetIds:    Set[TargetModel.Id]
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

    val targetIds: Lens[Default, Set[TargetModel.Id]] =
      Lens[Default, Set[TargetModel.Id]](_.targetIds)(a => b => b.copy(targetIds = a))

    val asterismTargetIds: Optional[AsterismModel, Set[TargetModel.Id]] =
      select ^|-> targetIds

  }

  trait Create[T] {
    def asterismId: Option[AsterismModel.Id]
    def programIds: List[ProgramModel.Id]  // to share immediately with the indicated programs
    def targetIds:  Set[TargetModel.Id]
    def withId:     ValidatedInput[AsterismModel.Id => T]
  }

  final case class CreateDefault(
    asterismId:   Option[AsterismModel.Id],
    programIds:   List[ProgramModel.Id],
    explicitBase: Option[CoordinatesModel.Input],
    targetIds:    Set[TargetModel.Id]
  ) extends Create[AsterismModel.Default] {

    override def withId: ValidatedInput[AsterismModel.Id => AsterismModel.Default] =
      explicitBase.traverse(_.toCoordinates).map(c => aid => Default(aid, Present, c, targetIds))

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

    def targetIds: Set[TargetModel.Id] =
      ifu2.foldLeft(Set(ifu1))(_ + _)

  }

  object Ghost {

    implicit val EqGhost: Eq[Ghost] =
      Eq.by(g => (g.id, g.existence, g.explicitBase, g.ifu1, g.ifu2))

  }

  final case class EditDefault(
    asterismId:   Id,
    existence:    Option[Existence],
    explicitBase: Option[Option[CoordinatesModel.Input]],
    targetIds:    Option[Set[TargetModel.Id]]
  ) extends Editor[Id, AsterismModel.Default] {

    override def id: Id =
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
    asterismId: Id,
    programIds: List[ProgramModel.Id]
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

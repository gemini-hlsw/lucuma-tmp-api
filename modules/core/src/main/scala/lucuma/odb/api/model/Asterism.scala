// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.Existence._
import lucuma.odb.api.model.syntax.all._
import lucuma.core.util.{Enumerated, Gid}
import lucuma.core.math.Coordinates

import cats.Eq
import cats.data.State
import cats.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.PosLong
import monocle.{Lens, Optional, Prism}

sealed trait Asterism {

  def id: Asterism.Id
  def existence: Existence

  def explicitBase: Option[Coordinates]
  def tpe:          Asterism.Type
  def targets:      List[Target.Id]

  def fold[B](
    default: Asterism.Default => B,
    ghost:   Asterism.Ghost   => B
  ): B =
    this match {
      case a: Asterism.Default => default(a)
      case a: Asterism.Ghost   => ghost(a)
    }

}

object Asterism extends AsterismOptics {

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

  implicit val TopLevelAsterism: TopLevel[Id, Asterism] =
    TopLevel.instance(_.id, Asterism.existence)

  final case class Default(
    id:           Asterism.Id,
    existence:    Existence,
    explicitBase: Option[Coordinates],
    targets:      List[Target.Id]
  ) extends Asterism {

    def tpe: Type =
      Type.Default

  }

  object Default extends DefaultOptics {

    implicit val EqDefault: Eq[Default] =
      Eq.by(d => (d.id, d.existence, d.explicitBase, d.targets))

  }

  trait DefaultOptics { self: Default.type =>

    val select: Prism[Asterism, Default] =
      Prism.partial[Asterism, Default]{case d: Default => d}(identity)

    val id: Lens[Default, Asterism.Id] =
      Lens[Default, Asterism.Id](_.id)(a => b => b.copy(id = a))

    val asterismId: Optional[Asterism, Asterism.Id] =
      select ^|-> id

    val existence: Lens[Default, Existence] =
      Lens[Default, Existence](_.existence)(a => b => b.copy(existence = a))

    val asterismExistence: Optional[Asterism, Existence] =
      select ^|-> existence

    val explicitBase: Lens[Default, Option[Coordinates]] =
      Lens[Default, Option[Coordinates]](_.explicitBase)(a => b => b.copy(explicitBase = a))

    val asterismExplicitBase: Optional[Asterism, Option[Coordinates]] =
      select ^|-> explicitBase

    val targets: Lens[Default, List[Target.Id]] =
      Lens[Default, List[Target.Id]](_.targets)(a => b => b.copy(targets = a))

    val asterismTargets: Optional[Asterism, List[Target.Id]] =
      select ^|-> targets

  }

  trait Create {
    def programs: List[Program.Id]  // to share immediately with the indicated programs
    def targets:  List[Target.Id]
    def withId(aid: Asterism.Id): Asterism
  }

  final case class CreateDefault(
    programs:     List[Program.Id],
    explicitBase: Option[Coordinates],
    targets:      List[Target.Id]
  ) extends Create {

    override def withId(aid: Asterism.Id): Asterism =
      Default(aid, Present, explicitBase, targets)

  }

  // not meant to be realistic yet
  final case class Ghost(
    id:           Asterism.Id,
    existence:    Existence,
    explicitBase: Option[Coordinates],
    ifu1:         Target.Id,
    ifu2:         Option[Target.Id]
  ) extends Asterism {

    def tpe: Type =
      Type.Ghost

    def targets: List[Target.Id] =
      ifu1 :: ifu2.toList

  }

  object Ghost {

    implicit val EqGhost: Eq[Ghost] =
      Eq.by(g => (g.id, g.existence, g.explicitBase, g.ifu1, g.ifu2))

  }

  final case class EditDefault(
    id:           Asterism.Id,
    existence:    Option[Existence],
    explicitBase: Option[Option[Coordinates]],
    targets:      Option[List[Target.Id]]
  ) extends Editor[Id, Asterism] {

    override def editor: State[Asterism, Unit] =
      for {
        _ <- Default.asterismExistence    := existence
        _ <- Default.asterismExplicitBase := explicitBase
        _ <- Default.asterismTargets      := targets
      } yield ()
  }

  implicit val EqAsterism: Eq[Asterism] =
    Eq.instance[Asterism] {
      case (a: Default, b: Default) => a === b
      case (a: Ghost, b: Ghost)     => a === b
      case (_, _)                   => false
    }

  final case class AsterismCreatedEvent (
    id: Long,
    value: Asterism,
  ) extends Event.Created[Asterism]

  object AsterismCreatedEvent {
    def apply(value: Asterism)(id: Long): AsterismCreatedEvent =
      AsterismCreatedEvent(id, value)
  }

  final case class AsterismEditedEvent (
    id: Long,
    oldValue: Asterism,
    newValue: Asterism
  ) extends Event.Edited[Asterism]

  object AsterismEditedEvent {
    def apply(oldValue: Asterism, newValue: Asterism)(id: Long): AsterismEditedEvent =
      AsterismEditedEvent(id, oldValue, newValue)
  }
}

trait AsterismOptics { self: Asterism.type =>

  val existence: Lens[Asterism, Existence] =
    Lens[Asterism, Existence](_.existence) { a =>
      _.fold(
        _.copy(existence = a),
        _.copy(existence = a)
      )
    }

}

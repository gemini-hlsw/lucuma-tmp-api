// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import lucuma.core.math.Coordinates
import cats.{Eq, Monad}
import cats.Order.catsKernelOrderingForOrder
import cats.data.{NonEmptyChain, State}
import cats.mtl.Stateful
import cats.syntax.apply._
import cats.syntax.eq._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.traverse._
import clue.data.Input
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.core.model.{Program, Target}
import lucuma.core.optics.syntax.all._
import lucuma.core.util.Gid
import lucuma.odb.api.model.{CoordinatesModel, DatabaseState, InputError, ValidatedInput}
import lucuma.odb.api.model.syntax.input._
import monocle.Lens

import scala.collection.immutable.SortedSet

final case class TargetEnvironmentModel(
  asterism:     SortedSet[Target.Id],
  explicitBase: Option[Coordinates]
) {

  def validate[F[_]: Monad, T](
    db: DatabaseState[T],
    pid: Program.Id
  )(implicit S: Stateful[F, T]): F[ValidatedInput[List[TargetModel]]] = {
    def err: NonEmptyChain[InputError] =
      NonEmptyChain.one(
        InputError.fromMessage(s"Cannot assign targets from programs other than ${Gid[Program.Id].show(pid)}")
      )

    db.target
      .lookupAllValidated(asterism.toList)
      .map(_.ensure(err)(_.forall(_.programId === pid)))
  }

}

object TargetEnvironmentModel extends TargetEnvironmentModelOptics {

  val empty: TargetEnvironmentModel =
    TargetEnvironmentModel(SortedSet.empty[Target.Id], None)

  implicit val EqTargetEnvironmentModel: Eq[TargetEnvironmentModel] =
    Eq.by { tem =>
      (
        tem.asterism,
        tem.explicitBase
      )
    }

  final case class Create(
    asterism:     Option[List[Target.Id]],
    explicitBase: Option[CoordinatesModel.Input]
  ) {

    def create[F[_]: Monad, T](
      db: DatabaseState[T]
    )(implicit S: Stateful[F, T]): F[ValidatedInput[TargetEnvironmentModel]] = {
      val scienceTargets = asterism.toList.flatten

      for {
        ts <- db.target.lookupAllValidated(scienceTargets)
        b   = explicitBase.traverse(_.toCoordinates)
        e   = (ts, b).mapN { (_, bʹ) => TargetEnvironmentModel(SortedSet.from(scienceTargets), bʹ) }
      } yield e
    }

  }

  object Create {

    val Empty: Create =
      Create(None, None)

    implicit val DecoderCreate: Decoder[Create] =
      deriveDecoder[Create]

    implicit val EqCreate: Eq[Create] =
      Eq.by { a => (
        a.asterism,
        a.explicitBase
      )}

  }

  final case class Edit(
    explicitBase: Input[CoordinatesModel.Input] = Input.ignore,
    asterism:     Option[List[Target.Id]]       = None
  ) {

    val editor: ValidatedInput[State[TargetEnvironmentModel, Unit]] =
      explicitBase.validateNullable(_.toCoordinates).map { b =>
        for {
          _ <- TargetEnvironmentModel.explicitBase := b
          _ <- TargetEnvironmentModel.asterism     := asterism.map(ts => SortedSet.from(ts)(catsKernelOrderingForOrder))
        } yield ()
      }
  }

  object Edit {

    def explicitBase(c: CoordinatesModel.Input): Edit =
      Edit(Input.assign(c), None)

    def asterism(tids: List[Target.Id]): Edit =
      Edit(Input.ignore, tids.some)

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderEdit: Decoder[Edit] =
      deriveConfiguredDecoder[Edit]

    implicit val EqEdit: Eq[Edit] =
      Eq.by { a => (
        a.explicitBase,
        a.asterism
      )}

  }

}


trait TargetEnvironmentModelOptics { self: TargetEnvironmentModel.type =>

  val asterism: Lens[TargetEnvironmentModel, SortedSet[Target.Id]] =
    Lens[TargetEnvironmentModel, SortedSet[Target.Id]](_.asterism)(a => _.copy(asterism = a))

  val explicitBase: Lens[TargetEnvironmentModel, Option[Coordinates]] =
    Lens[TargetEnvironmentModel, Option[Coordinates]](_.explicitBase)(a => _.copy(explicitBase = a))

}

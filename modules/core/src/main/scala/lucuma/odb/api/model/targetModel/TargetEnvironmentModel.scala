// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import lucuma.core.math.Coordinates
import cats.{Eq, Monad}
import cats.Order.catsKernelOrderingForOrder
import cats.data.State
import cats.mtl.Stateful
import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.traverse._
import clue.data.Input
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.core.model.Target
import lucuma.core.optics.syntax.all._
import lucuma.odb.api.model.{CoordinatesModel, DatabaseState, ValidatedInput}
import lucuma.odb.api.model.syntax.input._
import monocle.Lens

import scala.collection.immutable.SortedSet

final case class TargetEnvironmentModel(
  asterism:     SortedSet[Target.Id],
  explicitBase: Option[Coordinates]
)

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

    def editor[F[_]: Monad, T](
      db: DatabaseState[T]
    )(implicit S: Stateful[F, T]): F[ValidatedInput[State[TargetEnvironmentModel, Unit]]] =

      asterism.traverse(db.target.lookupAllValidated[F]).map { as =>
        (explicitBase.validateNullable(_.toCoordinates),
         as.sequence
        ).mapN { (b, _) =>
          for {
            _ <- TargetEnvironmentModel.explicitBase := b
            _ <- TargetEnvironmentModel.asterism     := asterism.map(ts => SortedSet.from(ts)(catsKernelOrderingForOrder))
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

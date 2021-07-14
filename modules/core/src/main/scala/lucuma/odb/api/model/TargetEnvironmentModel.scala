// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.math.Coordinates
import lucuma.core.model.Target
import lucuma.core.optics.state.all._
import lucuma.core.optics.syntax.lens._
import lucuma.odb.api.model.syntax.input._
import cats.Eq
import cats.data.State
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all._
import clue.data.Input
import eu.timepit.refined.types.string._
import eu.timepit.refined.cats.refTypeOrder
import io.circe.Decoder
import io.circe.generic.semiauto._
import monocle.Lens

import scala.collection.immutable.SortedMap


final case class TargetEnvironmentModel(
  explicitBase: Option[Coordinates],
  science:      SortedMap[NonEmptyString, Target]
  // guide stars, blind offset, etc. go here in the future
)

object TargetEnvironmentModel extends TargetEnvironmentModelOptics {

  implicit val EqTargetEnvironmentModel: Eq[TargetEnvironmentModel] =
    Eq.by { tem => (
      tem.explicitBase,
      tem.science
    )}

  final case class Create(
    explicitBase: Option[CoordinatesModel.Input],
    science:      List[TargetNewModel.Create]
  ) {

    val create: ValidatedInput[TargetEnvironmentModel] =
      (explicitBase.traverse(_.toCoordinates),
       science.traverse(_.toGemTarget)
      ).mapN { (b, s) =>
        TargetEnvironmentModel(b, SortedMap.from(s.fproductLeft(_.name)))
      }

  }

  object Create {

    implicit val DecoderCreate: Decoder[Create] =
      deriveDecoder[Create]

    implicit val EqCreate: Eq[Create] =
      Eq.by { a => (
        a.explicitBase,
        a.science
      )}

  }

  final case class Edit(
    explicitBase: Input[CoordinatesModel.Input] = Input.ignore,
    science:      Option[TargetNewModel.EditTargetList]
  ) {

    val editor: ValidatedInput[State[TargetEnvironmentModel, Unit]] =
      (explicitBase.validateNullable(_.toCoordinates),
       science.traverse(_.editor("science"))
      ).mapN { (b, s) =>
        for {
          _ <- TargetEnvironmentModel.explicitBase := b
          _ <- s.fold(State.get[TargetEnvironmentModel].void) { ed =>
            TargetEnvironmentModel.science.mod_(ed.runS(_).value)
          }
        } yield ()
      }

  }

  object Target {

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderEdit: Decoder[Edit] =
      deriveConfiguredDecoder[Edit]

    implicit val EqEdit: Eq[Edit] =
      Eq.by { a => (
        a.explicitBase,
        a.science
      )}

  }
}



trait TargetEnvironmentModelOptics { self: TargetEnvironmentModel.type =>

  val explicitBase: Lens[TargetEnvironmentModel, Option[Coordinates]] =
    Lens[TargetEnvironmentModel, Option[Coordinates]](_.explicitBase)(a => _.copy(explicitBase = a))

  val science: Lens[TargetEnvironmentModel, SortedMap[NonEmptyString, Target]] =
    Lens[TargetEnvironmentModel, SortedMap[NonEmptyString, Target]](_.science)(a => _.copy(science = a))

}

// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.math.Coordinates
import lucuma.core.model.{Observation, Target}
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
import lucuma.odb.api.model.TargetModel.EditTargetList
import monocle.Lens

import scala.collection.immutable.SortedMap


final case class TargetEnvironmentModel(
  explicitBase: Option[Coordinates],
  science:      SortedMap[NonEmptyString, Target]
  // guide stars, blind offset, etc. go here in the future
)

object TargetEnvironmentModel extends TargetEnvironmentModelOptics {

  val Empty: TargetEnvironmentModel =
    TargetEnvironmentModel(None, SortedMap.empty)

  implicit val EqTargetEnvironmentModel: Eq[TargetEnvironmentModel] =
    Eq.by { tem => (
      tem.explicitBase,
      tem.science
    )}

  final case class Create(
    explicitBase: Option[CoordinatesModel.Input],
    science:      List[TargetModel.Create]
  ) {

    val create: ValidatedInput[TargetEnvironmentModel] = {

      val listEditor = EditTargetList.replace(science)

      (explicitBase.traverse(_.toCoordinates),
       listEditor.targetMapEditor("science"),
       listEditor.validateObservationEdit(Set.empty, "science", Option.empty)
      ).mapN { (b, sf, _) =>
        TargetEnvironmentModel(b, sf(SortedMap.empty[NonEmptyString, Target]))
      }
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

    def single(science: TargetModel.Create): Create =
      Create(None, List(science))

    def singleNonsidereal(nonsidereal: TargetModel.CreateNonsidereal): Create =
      Create(None, List(TargetModel.Create.nonsidereal(nonsidereal)))

    def singleSidereal(sidereal: TargetModel.CreateSidereal): Create =
      Create(None, List(TargetModel.Create.sidereal(sidereal)))

    def fromSidereal(cs: IterableOnce[TargetModel.CreateSidereal]): Create =
      Create(None, cs.iterator.map(TargetModel.Create.sidereal).toList)

    def fromNonsidereal(cs: IterableOnce[TargetModel.CreateNonsidereal]): Create =
      Create(None, cs.iterator.map(TargetModel.Create.nonsidereal).toList)

  }

  final case class Edit(
    explicitBase: Input[CoordinatesModel.Input]      = Input.ignore,
    science:      Option[TargetModel.EditTargetList]
  ) {

    def validateObservationEdit(
      targetEnv:     TargetEnvironmentModel,
      observationId: Option[Observation.Id]
    ): ValidatedInput[Unit] =
      science.traverse(_.validateObservationEdit(targetEnv.science.keySet, "science", observationId)).void

    val editor: ValidatedInput[State[TargetEnvironmentModel, Unit]] =
      (explicitBase.validateNullable(_.toCoordinates),
       science.traverse(_.targetMapEditor("science"))
      ).mapN { (b, e) =>
        for {
          _ <- TargetEnvironmentModel.explicitBase := b
          _ <- TargetEnvironmentModel.science.mod(e.getOrElse(identity))
        } yield ()
      }

  }

  object Edit {

    val Empty: Edit =
      Edit(explicitBase = Input.ignore, science = None)

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

    def explicitBase(
      ra:  RightAscensionModel.Input,
      dec: DeclinationModel.Input
    ): Edit =
      Empty.copy(explicitBase = Input.assign(CoordinatesModel.Input(ra, dec)))

  }
}



trait TargetEnvironmentModelOptics { self: TargetEnvironmentModel.type =>

  val explicitBase: Lens[TargetEnvironmentModel, Option[Coordinates]] =
    Lens[TargetEnvironmentModel, Option[Coordinates]](_.explicitBase)(a => _.copy(explicitBase = a))

  val science: Lens[TargetEnvironmentModel, SortedMap[NonEmptyString, Target]] =
    Lens[TargetEnvironmentModel, SortedMap[NonEmptyString, Target]](_.science)(a => _.copy(science = a))

}

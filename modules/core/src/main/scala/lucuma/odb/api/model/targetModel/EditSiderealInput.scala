// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.Eq
import cats.data.State
import cats.syntax.apply._
import cats.syntax.traverse._
import clue.data.Input
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.refined._
import lucuma.core.math.Epoch
import lucuma.core.model.Target
import lucuma.core.optics.state.all._
import lucuma.core.optics.syntax.optional._
import lucuma.odb.api.model.{CatalogIdModel, DeclinationModel, MagnitudeModel, ParallaxModel, ProperMotionModel, RadialVelocityModel, RightAscensionModel, ValidatedInput}
import lucuma.odb.api.model.json.target._
import lucuma.odb.api.model.syntax.input._


final case class EditSiderealInput(
  select:           SelectTargetInput,
  name:             Input[NonEmptyString]            = Input.ignore,
  catalogId:        Input[CatalogIdModel.Input]      = Input.ignore,
  ra:               Input[RightAscensionModel.Input] = Input.ignore,
  dec:              Input[DeclinationModel.Input]    = Input.ignore,
  epoch:            Input[Epoch]                     = Input.ignore,
  properMotion:     Input[ProperMotionModel.Input]   = Input.ignore,
  radialVelocity:   Input[RadialVelocityModel.Input] = Input.ignore,
  parallax:         Input[ParallaxModel.Input]       = Input.ignore,
  magnitudes:       Option[MagnitudeModel.EditList],
) extends TargetEditor {

  override val editor: ValidatedInput[State[Target, Unit]] =
    (name          .validateIsNotNull("name"),
     catalogId     .validateNullable(_.toCatalogId),
     ra            .validateNotNullable("ra")(_.toRightAscension),
     dec           .validateNotNullable("dec")(_.toDeclination),
     epoch         .validateIsNotNull("epoch"),
     properMotion  .validateNullable(_.toProperMotion),
     radialVelocity.validateNullable(_.toRadialVelocity),
     parallax      .validateNullable(_.toParallax),
     magnitudes    .traverse(_.editor)
    ).mapN { (name, catalogId, ra, dec, epoch, pm, rv, px, ms) =>
      for {
        _ <- TargetModel.name           := name
        _ <- TargetModel.catalogId      := catalogId
        _ <- TargetModel.ra             := ra
        _ <- TargetModel.dec            := dec
        _ <- TargetModel.epoch          := epoch
        _ <- TargetModel.properMotion   := pm
        _ <- TargetModel.radialVelocity := rv
        _ <- TargetModel.parallax       := px
        _ <- TargetModel.magnitudes.mod(m => ms.fold(m)(_.runS(m).value))
      } yield ()
    }

}

object EditSiderealInput {

  import io.circe.generic.extras.semiauto._
  import io.circe.generic.extras.Configuration
  implicit val customConfig: Configuration = Configuration.default.withDefaults


  implicit val DecoderEditSidereal: Decoder[EditSiderealInput] =
    deriveConfiguredDecoder[EditSiderealInput]

  implicit val EqEditSidereal: Eq[EditSiderealInput] =
    Eq.by(es => (
      es.select,
      es.name,
      es.catalogId,
      es.ra,
      es.dec,
      es.epoch,
      es.properMotion,
      es.radialVelocity,
      es.parallax,
      es.magnitudes
    ))

}

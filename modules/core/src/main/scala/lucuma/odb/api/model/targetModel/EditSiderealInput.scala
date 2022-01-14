// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.Eq
import cats.data.State
import cats.syntax.apply._
import clue.data.Input
import eu.timepit.refined.cats._
import io.circe.Decoder
import lucuma.core.math.Epoch
import lucuma.core.model.Target
import lucuma.core.optics.syntax.optional._
import lucuma.odb.api.model.{CatalogInfoModel, DeclinationModel, ParallaxModel, ProperMotionModel, RadialVelocityModel, RightAscensionModel, ValidatedInput}
import lucuma.odb.api.model.json.target._
import lucuma.odb.api.model.syntax.input._


final case class EditSiderealInput(
  catalogInfo:      Input[CatalogInfoModel.Input]    = Input.ignore,
  ra:               Input[RightAscensionModel.Input] = Input.ignore,
  dec:              Input[DeclinationModel.Input]    = Input.ignore,
  epoch:            Input[Epoch]                     = Input.ignore,
  properMotion:     Input[ProperMotionModel.Input]   = Input.ignore,
  radialVelocity:   Input[RadialVelocityModel.Input] = Input.ignore,
  parallax:         Input[ParallaxModel.Input]       = Input.ignore
) {

  val editor: ValidatedInput[State[Target, Unit]] =
    (catalogInfo   .validateNullable(_.toCatalogInfo),
     ra            .validateNotNullable("ra")(_.toRightAscension),
     dec           .validateNotNullable("dec")(_.toDeclination),
     epoch         .validateIsNotNull("epoch"),
     properMotion  .validateNullable(_.toProperMotion),
     radialVelocity.validateNullable(_.toRadialVelocity),
     parallax      .validateNullable(_.toParallax)
    ).mapN { (catalogInfo, ra, dec, epoch, pm, rv, px) =>
      for {
        _ <- Target.catalogInfo       := catalogInfo
        _ <- Target.baseRA            := ra
        _ <- Target.baseDec           := dec
        _ <- Target.epoch             := epoch
        _ <- Target.properMotion      := pm
        _ <- Target.radialVelocity    := rv
        _ <- Target.parallax          := px
//        _ <- Target.magnitudes.mod(m => ms.fold(m)(_.runS(m).value))
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
      es.catalogInfo,
      es.ra,
      es.dec,
      es.epoch,
      es.properMotion,
      es.radialVelocity,
      es.parallax
    ))

}


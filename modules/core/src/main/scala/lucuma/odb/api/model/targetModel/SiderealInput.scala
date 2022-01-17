// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.Eq
import cats.data.StateT
import cats.syntax.all._
import clue.data.Input
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.refined._
import lucuma.core.math.{Coordinates, Epoch}
import lucuma.core.model.{SiderealTracking, Target}
import lucuma.core.model.Target.Sidereal
import lucuma.odb.api.model.{DeclinationModel, EditorInput, EitherInput, ParallaxModel, ProperMotionModel, RadialVelocityModel, RightAscensionModel, ValidatedInput}
import lucuma.odb.api.model.json.target._
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.optional._
import lucuma.odb.api.model.syntax.prism._
import lucuma.odb.api.model.targetModel.SourceProfileModel.CreateSourceProfileInput


final case class SiderealInput(
  name:             Input[NonEmptyString]             = Input.ignore,
  ra:               Input[RightAscensionModel.Input]  = Input.ignore,
  dec:              Input[DeclinationModel.Input]     = Input.ignore,
  epoch:            Input[Epoch]                      = Input.ignore,
  properMotion:     Input[ProperMotionModel.Input]    = Input.ignore,
  radialVelocity:   Input[RadialVelocityModel.Input]  = Input.ignore,
  parallax:         Input[ParallaxModel.Input]        = Input.ignore,
  sourceProfile:    Input[CreateSourceProfileInput]   = Input.ignore,
  catalogInfo:      Input[CatalogInfoInput]           = Input.ignore
) extends EditorInput[Sidereal] {

  val toSiderealTracking: ValidatedInput[SiderealTracking] =
    (ra.notMissingAndThen("sidereal 'ra'")(_.toRightAscension),
     dec.notMissingAndThen("sidereal 'dec'")(_.toDeclination),
     properMotion.toOption.traverse(_.toProperMotion),
     radialVelocity.toOption.traverse(_.toRadialVelocity),
     parallax.toOption.traverse(_.toParallax)
    ).mapN { (ra, dec, pm, rv, px) =>
      SiderealTracking(
        Coordinates(ra, dec),
        epoch.toOption.getOrElse(Epoch.J2000),
        pm,
        rv,
        px
      )
    }

  override val create: ValidatedInput[Sidereal] =
    (name.notMissing("sidereal 'name'"),
     toSiderealTracking,
     sourceProfile.notMissingAndThen("sidereal 'sourceProfile'")(_.toSourceProfile),
     catalogInfo.toOption.traverse(_.create)
    ).mapN { (n, t, s, c) =>
      Sidereal(n, t, s, c)
    }

  override val edit: StateT[EitherInput, Sidereal, Unit] = {
    val validArgs =
      (name .validateIsNotNull("name"),
       ra   .validateNotNullable("ra")(_.toRightAscension),
       dec  .validateNotNullable("dec")(_.toDeclination),
       epoch.validateIsNotNull("epoch"),
       properMotion  .validateNullable(_.toProperMotion),
       radialVelocity.validateNullable(_.toRadialVelocity),
       parallax      .validateNullable(_.toParallax)
      ).tupled.toEither

    for {
      args <- StateT.liftF(validArgs)
      (n, r, d, e, pm, rv, px) = args

      _ <- Sidereal.name           := n
      _ <- Sidereal.baseRA         := r
      _ <- Sidereal.baseDec        := d
      _ <- Sidereal.epoch          := e
      _ <- Sidereal.properMotion   := pm
      _ <- Sidereal.radialVelocity := rv
      _ <- Sidereal.parallax       := px
//    _ <- EditorInput.notNullable(Sidereal.sourceProfile.asOptional, sourceProfile) TBD
      _ <- EditorInput.nullable(Sidereal.catalogInfo.asOptional, catalogInfo)
    } yield ()

  }

  val targetEditor: StateT[EitherInput, Target, Unit] =
    Target.sidereal.transformOrIgnore(edit)

}

object SiderealInput {

  import io.circe.generic.extras.semiauto._
  import io.circe.generic.extras.Configuration
  implicit val customConfig: Configuration = Configuration.default.withDefaults

  implicit val DecoderEditSidereal: Decoder[SiderealInput] =
    deriveConfiguredDecoder[SiderealInput]

  implicit val EqEditSidereal: Eq[SiderealInput] =
    Eq.by { a => (
      a.name,
      a.ra,
      a.dec,
      a.epoch,
      a.properMotion,
      a.radialVelocity,
      a.parallax,
      a.sourceProfile,
      a.catalogInfo,
    )}

}


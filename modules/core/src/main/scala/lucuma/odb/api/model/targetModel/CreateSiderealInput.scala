// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.Eq
import cats.syntax.apply._
import cats.syntax.traverse._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.refined._
import lucuma.core.math.{Coordinates, Epoch}
import lucuma.core.model.{SiderealTracking, Target}
import lucuma.odb.api.model.{CatalogInfoModel, DeclinationModel, ParallaxModel, ProperMotionModel, RadialVelocityModel, RightAscensionModel, ValidatedInput}
import lucuma.odb.api.model.json.target._
import lucuma.odb.api.model.targetModel.SourceProfileModel.CreateSourceProfileInput

/**
 * Describes input used to create a sidereal target.
 */
final case class CreateSiderealInput(
  name:           NonEmptyString,
  catalogInfo:    Option[CatalogInfoModel.Input],
  ra:             RightAscensionModel.Input,
  dec:            DeclinationModel.Input,
  epoch:          Option[Epoch],
  properMotion:   Option[ProperMotionModel.Input],
  radialVelocity: Option[RadialVelocityModel.Input],
  parallax:       Option[ParallaxModel.Input],
  sourceProfile:  CreateSourceProfileInput
) {

  val toSiderealTracking: ValidatedInput[SiderealTracking] =
    (ra.toRightAscension,
     dec.toDeclination,
     properMotion.traverse(_.toProperMotion),
     radialVelocity.traverse(_.toRadialVelocity),
     parallax.traverse(_.toParallax)
    ).mapN { (ra, dec, pm, rv, px) =>
      SiderealTracking(
        Coordinates(ra, dec),
        epoch.getOrElse(Epoch.J2000),
        pm,
        rv,
        px
      )
    }

  val toGemTarget: ValidatedInput[Target] =
    (catalogInfo.traverse(_.toCatalogInfo),
     toSiderealTracking,
     sourceProfile.toSourceProfile
    ).mapN { (ci, pm, sp) =>
      Target.Sidereal(
        name,
        pm,
        sp,
        ci,
        None
      )
    }

}

object CreateSiderealInput {

  def fromRaDec(
    name:   NonEmptyString,
    ra:     RightAscensionModel.Input,
    dec:    DeclinationModel.Input,
    source: CreateSourceProfileInput
  ): CreateSiderealInput =
    CreateSiderealInput(
      name           = name,
      catalogInfo    = None,
      ra             = ra,
      dec            = dec,
      epoch          = None,
      properMotion   = None,
      radialVelocity = None,
      parallax       = None,
      sourceProfile  = source
    )

  implicit val DecoderCreateSiderealInput: Decoder[CreateSiderealInput] =
    deriveDecoder[CreateSiderealInput]

  implicit val EqCreateSidereal: Eq[CreateSiderealInput] =
    Eq.by { a => (
      a.name,
      a.catalogInfo,
      a.ra,
      a.dec,
      a.epoch,
      a.properMotion,
      a.radialVelocity,
      a.parallax,
      a.sourceProfile
    )}

}


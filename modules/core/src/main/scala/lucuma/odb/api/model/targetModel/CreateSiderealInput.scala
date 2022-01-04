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
import lucuma.core.`enum`.{Band, PlanetSpectrum}
import lucuma.core.math.BrightnessUnits.{Brightness, Integrated}
import lucuma.core.math.{BrightnessValue, Coordinates, Epoch}
import lucuma.core.model.{BandBrightness, SiderealTracking, SourceProfile, SpectralDefinition, Target, UnnormalizedSED}
import lucuma.odb.api.model.{CatalogInfoModel, DeclinationModel, ParallaxModel, ProperMotionModel, RadialVelocityModel, RightAscensionModel, ValidatedInput}
import lucuma.odb.api.model.json.target._

import scala.collection.immutable.SortedMap

/**
 * Describes input used to create a sidereal target.
 *
 * @param name target name
 * @param ra right ascension coordinate at epoch
 * @param dec declination coordinate at epoch
 * @param epoch time of the base observation
 * @param properMotion proper motion per year in right ascension and declination
 * @param radialVelocity radial velocity
 * @param parallax parallax
 */
final case class CreateSiderealInput(
  name:           NonEmptyString,
  catalogInfo:    Option[CatalogInfoModel.Input],
  ra:             RightAscensionModel.Input,
  dec:            DeclinationModel.Input,
  epoch:          Option[Epoch],
  properMotion:   Option[ProperMotionModel.Input],
  radialVelocity: Option[RadialVelocityModel.Input],
  parallax:       Option[ParallaxModel.Input]
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
     toSiderealTracking
    ).mapN { (ci, pm) =>

      // Temporary pending proper Input types
      val r: Band = Band.R
      val bv = BrightnessValue.fromDouble(10.0)
      val ms = Brightness.Integrated.all.head.withValueTagged(bv)
      val bb = BandBrightness[Integrated](ms, r)

      Target.Sidereal(
        name,
        pm,
        // Nonsense value to satisfy the compiler for now.
        SourceProfile.Point(
          SpectralDefinition.BandNormalized(
            UnnormalizedSED.Planet(PlanetSpectrum.Mars),
            SortedMap.from[Band, BandBrightness[Integrated]](List(r -> bb)))
        ),
        ci,
        None
      )
    }

}

object CreateSiderealInput {

  def fromRaDec(
    name: NonEmptyString,
    ra:   RightAscensionModel.Input,
    dec:  DeclinationModel.Input
  ): CreateSiderealInput =
    CreateSiderealInput(
      name           = name,
      catalogInfo    = None,
      ra             = ra,
      dec            = dec,
      epoch          = None,
      properMotion   = None,
      radialVelocity = None,
      parallax       = None
    )

  implicit val DecoderCreateSiderealInput: Decoder[CreateSiderealInput] =
    deriveDecoder[CreateSiderealInput]

  implicit val EqCreateSidereal: Eq[CreateSiderealInput] =
    Eq.by(cs => (
      cs.name,
      cs.catalogInfo,
      cs.ra,
      cs.dec,
      cs.epoch,
      cs.properMotion,
      cs.radialVelocity,
      cs.parallax
    ))

}


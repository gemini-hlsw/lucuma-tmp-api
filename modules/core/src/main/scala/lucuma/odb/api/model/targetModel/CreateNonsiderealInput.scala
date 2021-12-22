// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.Eq
import cats.syntax.apply._
import cats.syntax.option._
import cats.syntax.traverse._
import lucuma.core.math.dimensional.GroupedUnitOfMeasure
import lucuma.core.math.units.VegaMagnitude
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.refined._
import lucuma.core.`enum`.{Band, EphemerisKeyType, PlanetSpectrum}
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.BrightnessValue
import lucuma.core.model.{BandBrightness, EphemerisKey, SourceProfile, SpectralDefinition, Target, UnnormalizedSpectralEnergyDistribution}
import lucuma.odb.api.model.{InputError, MagnitudeModel, ValidatedInput}

import scala.collection.immutable.SortedMap

/**
 * Describes input used to create a nonsidereal target.
 *
 * @param name    target name
 * @param keyType ephemeris key type
 * @param des     semi-permanent horizons identifier (relative to key type)
 */
final case class CreateNonsiderealInput(
  name:       NonEmptyString,
  keyType:    EphemerisKeyType,
  des:        String,
  magnitudes: Option[List[MagnitudeModel.Create]]
) {

  val toEphemerisKey: ValidatedInput[EphemerisKey] =
    CreateNonsiderealInput.parse.ephemerisKey("des", keyType, des)

  val toGemTarget: ValidatedInput[Target] =
    (toEphemerisKey,
     magnitudes.toList.flatten.traverse(_.toMagnitude)
    ).mapN { (k, _) =>
      Target.Nonsidereal(
        name,
        k,
        // Nonsense value to satisfy the compiler for now.
        SourceProfile.Point(
          SpectralDefinition.BandNormalized(
            UnnormalizedSpectralEnergyDistribution.Planet(PlanetSpectrum.Mars),
            SortedMap.from[Band, BandBrightness[Integrated]](
              List(
                (
                  Band.R: Band,
                  BandBrightness(
                    GroupedUnitOfMeasure[Brightness[Integrated], VegaMagnitude].withValue(BrightnessValue.fromDouble(10.0)),
                    Band.R: Band,
                  )
                )
              )
            )
          )
        ),
//        SortedMap.from(ms.fproductLeft(_.band)),
        None
      )
    }

}

object CreateNonsiderealInput {

  implicit val DecoderCreateNonsiderealInput: Decoder[CreateNonsiderealInput] =
    deriveDecoder[CreateNonsiderealInput]

  implicit val EqCreateNonsiderealInput: Eq[CreateNonsiderealInput] =
    Eq.by(cn => (
      cn.name,
      cn.keyType,
      cn.des,
      cn.magnitudes
    ))

    object parse {

    def ephemerisKey(
      fieldName: String,
      key:       EphemerisKeyType,
      input:     String
    ): ValidatedInput[EphemerisKey] =
      EphemerisKey
        .fromTypeAndDes
        .getOption((key, input))
        .toValidNec(
          InputError.invalidField(fieldName, input, s"Invalid description for ephemeris key type `${key.shortName}`")
        )

  }

}

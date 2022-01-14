// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.Eq
import cats.syntax.apply._
import cats.syntax.option._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.refined._
import lucuma.core.`enum`.EphemerisKeyType
import lucuma.core.model.{EphemerisKey, Target}
import lucuma.odb.api.model.targetModel.SourceProfileModel.CreateSourceProfileInput
import lucuma.odb.api.model.{InputError, ValidatedInput}

/**
 * Describes input used to create a nonsidereal target.
 *
 * @param name    target name
 * @param keyType ephemeris key type
 * @param des     semi-permanent horizons identifier (relative to key type)
 */
final case class CreateNonsiderealInput(
  name:          NonEmptyString,
  keyType:       EphemerisKeyType,
  des:           String,
  sourceProfile: CreateSourceProfileInput
) {

  val toEphemerisKey: ValidatedInput[EphemerisKey] =
    CreateNonsiderealInput.parse.ephemerisKey("des", keyType, des)

  val toGemTarget: ValidatedInput[Target] =
    (toEphemerisKey, sourceProfile.toSourceProfile).mapN { (k, s) =>

      Target.Nonsidereal(
        name,
        k,
        s,
        None
      )
    }

}

object CreateNonsiderealInput {

  implicit val DecoderCreateNonsiderealInput: Decoder[CreateNonsiderealInput] =
    deriveDecoder[CreateNonsiderealInput]

  implicit val EqCreateNonsiderealInput: Eq[CreateNonsiderealInput] =
    Eq.by { a => (
      a.name,
      a.keyType,
      a.des,
      a.sourceProfile
    )}

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

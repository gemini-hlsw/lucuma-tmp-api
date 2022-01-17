// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.Eq
import cats.data.{EitherNec, StateT}
import cats.syntax.apply._
import cats.syntax.option._
import cats.syntax.validated._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.refined._
import lucuma.core.`enum`.EphemerisKeyType
import lucuma.core.model.Target.Nonsidereal
import lucuma.core.model.{EphemerisKey, SourceProfile, Target}
import lucuma.odb.api.model.{EitherInput, InputError, ValidatedInput}
import lucuma.odb.api.model.syntax.lens._
import lucuma.odb.api.model.syntax.prism._


/**
 * Describes input used to create or edit a nonsidereal target.
 *
 * @param keyType ephemeris key type
 * @param des     semi-permanent horizons identifier (relative to key type)
 * @param key     combination keyType and des in a single input
 */
final case class NonsiderealInput(
  keyType: Option[EphemerisKeyType],
  des:     Option[NonEmptyString],
  key:     Option[NonEmptyString]
) {

  val toEphemerisKey: ValidatedInput[EphemerisKey] = {
    (keyType, des, key) match {
      case (Some(t), Some(d), None  ) =>
        NonsiderealInput.key.fromTypeAndDes("des", t, d)
      case (None,    None,   Some(s)) =>
        NonsiderealInput.key.fromString("keyString", s)
      case _                          =>
        NonsiderealInput.Error.invalidNec[EphemerisKey]
    }
  }

  def createGemTarget(
    name:          NonEmptyString,
    sourceProfile: ValidatedInput[SourceProfile]
  ): ValidatedInput[Target] =
    (toEphemerisKey, sourceProfile).mapN { (k, s) =>

      Target.Nonsidereal(
        name,
        k,
        s
      )
    }

  def modType(keyType: EphemerisKeyType): StateT[EitherInput, EphemerisKey, Unit] =
    StateT.modifyF { k =>
      EphemerisKey.fromTypeAndDes.getOption((keyType, k.des)).toRightNec(
        InputError.fromMessage(s"""supplied `keyType` $keyType and existing des "${k.des}" do not combine to form a valid ephemeris key""")
      )
    }

  def modDes(des: NonEmptyString): StateT[EitherInput, EphemerisKey, Unit] =
    StateT.modifyF { k =>
      EphemerisKey.fromTypeAndDes.getOption((k.keyType, des.value)).toRightNec(
        InputError.fromMessage(s"""supplied `des` "${des.value}" and existing key type ${k.keyType} do not combine to form a valid ephemeris key""")
      )
    }

  def modTypeAndDes(keyType: EphemerisKeyType, des: NonEmptyString): StateT[EitherInput, EphemerisKey, Unit] =
    StateT.setF {
      EphemerisKey.fromTypeAndDes.getOption((keyType, des.value)).toRightNec(
        InputError.fromMessage(s"""supplied `keyType` $keyType and `des` ${des.value} do not combine to form a valid ephemeris key""")
      )
    }

  def modKey(key: NonEmptyString): StateT[EitherInput, EphemerisKey, Unit] =
    StateT.setF {
      EphemerisKey.fromString.getOption(key.value).toRightNec(
        InputError.fromMessage(s"""cannot parse supplied `key` "${key.value}" as an ephemeris key""")
      )
    }

  val editor: StateT[EitherNec[InputError, *], Target, Unit] = {

    val ed: StateT[EitherInput, EphemerisKey, Unit] =
      (keyType, des, key) match {
        case (Some(t), None,    None   ) => modType(t)
        case (None,    Some(d), None   ) => modDes(d)
        case (Some(t), Some(d), None   ) => modTypeAndDes(t, d)
        case (None,    None,    Some(k)) => modKey(k)
        case _                           => StateT.empty[EitherInput, EphemerisKey, Unit]
      }

    Target.nonsidereal.transformOrIgnore(Nonsidereal.ephemerisKey.transform(ed))
  }

}

object NonsiderealInput {

  private val Error: InputError =
    InputError.fromMessage("Provide both `keyType` and `des`, or else `key`")


  implicit val DecoderNonsiderealInput: Decoder[NonsiderealInput] =
    deriveDecoder[NonsiderealInput]

  implicit val EqNonsiderealInput: Eq[NonsiderealInput] =
    Eq.by { a => (
      a.keyType,
      a.des,
      a.key
    )}

  object key {

    def fromTypeAndDes(
      fieldName: String,
      key:       EphemerisKeyType,
      des:       NonEmptyString
    ): ValidatedInput[EphemerisKey] =
      EphemerisKey
        .fromTypeAndDes
        .getOption((key, des.value))
        .toValidNec(
          InputError.invalidField(fieldName, des.value, s"Invalid description for ephemeris key type `${key.shortName}`")
        )

    def fromString(
      fieldName: String,
      key:       NonEmptyString
    ): ValidatedInput[EphemerisKey] =
      EphemerisKey
        .fromString
        .getOption(key.value)
        .toValidNec(
          InputError.invalidField(fieldName, key.value, s"Invalid ephemeris key string")
        )
  }
}

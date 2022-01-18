// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.Eq
import cats.data.{EitherNec, StateT}
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.validated._
import clue.data.Input
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.refined._
import lucuma.core.`enum`.EphemerisKeyType
import lucuma.core.model.Target.Nonsidereal
import lucuma.core.model.{EphemerisKey, Target}
import lucuma.odb.api.model.{EditorInput, EitherInput, InputError, ValidatedInput}
import lucuma.odb.api.model.syntax.lens._
import lucuma.odb.api.model.syntax.prism._
import lucuma.odb.api.model.targetModel.SourceProfileModel.CreateSourceProfileInput


/**
 * Describes input used to create or edit a nonsidereal target.
 *
 * @param keyType ephemeris key type
 * @param des     semi-permanent horizons identifier (relative to key type)
 * @param key     combination keyType and des in a single input
 */
final case class NonsiderealInput(
  keyType:       Input[EphemerisKeyType] = Input.ignore,
  des:           Input[NonEmptyString]   = Input.ignore,
  key:           Input[NonEmptyString]   = Input.ignore,
) extends EditorInput[EphemerisKey] {

  override val create: ValidatedInput[EphemerisKey] =
    (keyType.toOption, des.toOption, key.toOption) match {
      case (Some(t), Some(d), None  ) =>
        NonsiderealInput.key.fromTypeAndDes("des", t, d)
      case (None,    None,   Some(s)) =>
        NonsiderealInput.key.fromString("keyString", s)
      case _                          =>
        NonsiderealInput.Error.invalidNec[EphemerisKey]
    }

  def createTarget(
    name:          NonEmptyString,
    sourceProfile: CreateSourceProfileInput
  ): ValidatedInput[Target] =
    (create, sourceProfile.toSourceProfile).mapN { (key, profile) =>
      Target.Nonsidereal(name, key, profile)
    }

  override val edit: StateT[EitherInput, EphemerisKey, Unit] = {
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

    (keyType.toOption, des.toOption, key.toOption) match {
      case (Some(t), None,    None   ) => modType(t)
      case (None,    Some(d), None   ) => modDes(d)
      case (Some(t), Some(d), None   ) => modTypeAndDes(t, d)
      case (None,    None,    Some(k)) => modKey(k)
      case (None,    None,    None   ) => StateT.empty[EitherInput, EphemerisKey, Unit]
      case _                           => StateT.setF(InputError.fromMessage("either specify `key` alone or else `keyType`/`des`").leftNec)
    }
  }

  val targetEditor: StateT[EitherNec[InputError, *], Target, Unit] =
    Target.nonsidereal.transformOrIgnore(
      Nonsidereal.ephemerisKey.transform(edit)
    )

}

object NonsiderealInput {

  private val Error: InputError =
    InputError.fromMessage("Provide both `keyType` and `des`, or else `key`")

  import io.circe.generic.extras.semiauto._
  import io.circe.generic.extras.Configuration
  implicit val customConfig: Configuration = Configuration.default.withDefaults

  implicit val DecoderNonsiderealInput: Decoder[NonsiderealInput] =
    deriveConfiguredDecoder[NonsiderealInput]

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

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.math.{Angle, HourAngle, RightAscension}
import lucuma.core.optics.SplitMono
import lucuma.core.util.{Display, Enumerated}
import cats.syntax.option._
import cats.syntax.validated._

import io.circe.Decoder
import io.circe.generic.semiauto._

object RightAscensionApi {

  sealed abstract class Units(
    val angleUnit: AngleApi.Units
  ) extends Product with Serializable {

    private def angleToRightAscension[A](m: SplitMono[Angle, A]): SplitMono[RightAscension, A] =
      m.imapA(
        a => RightAscension(HourAngle.fromMicroseconds((a.toMicroarcseconds + 7L)/15L)),
        _.toAngle
      )

    val long: SplitMono[RightAscension, Long] =
      angleToRightAscension(angleUnit.unsignedLong)

    def readLong(l: Long): ValidatedInput[RightAscension] =
      long.reverseGet(l).validNec[InputError]

    val decimal: SplitMono[RightAscension, BigDecimal] =
      angleToRightAscension(angleUnit.unsignedDecimal)

    def readDecimal(b: BigDecimal): ValidatedInput[RightAscension] =
      decimal.reverseGet(b).validNec[InputError]

  }

  object Units {

    case object Microarcseconds extends Units(AngleApi.Units.Milliarcseconds)
    case object Degrees         extends Units(AngleApi.Units.Degrees)
    case object Hours           extends Units(AngleApi.Units.Hours)

    implicit val EnumeratedRightAscensionUnits: Enumerated[Units] =
      Enumerated.of(Microarcseconds, Degrees, Hours)

    implicit val DisplayRightAscensionUnits: Display[Units] =
      Display.by(_.angleUnit.abbreviation, _.angleUnit.name)

  }

  final case class LongInput(
    value: Long,
    units: Units
  ) {

    val read: ValidatedInput[RightAscension] =
      units.readLong(value)

  }

  object LongInput {

    implicit val DecoderLongInput: Decoder[LongInput] =
      deriveDecoder[LongInput]

  }

  final case class DecimalInput(
    value: BigDecimal,
    units: Units
  ) {

    val read: ValidatedInput[RightAscension] =
      units.readDecimal(value)

  }

  object DecimalInput {

    implicit val DecoderDecimalInput: Decoder[DecimalInput] =
      deriveDecoder[DecimalInput]
  }

  def readHms(s: String): ValidatedInput[RightAscension] =
    RightAscension.fromStringHMS
      .getOption(s)
      .toValidNec(
        InputError.fromMessage(s"Could not parse $s as an HMS string.")
      )

  final case class Input(
    microarcseconds: Option[Long],
    degrees:         Option[BigDecimal],
    hours:           Option[BigDecimal],
    hms:             Option[String],
    fromLong:        Option[LongInput],
    fromDecimal:     Option[DecimalInput]
  ) {

    import Units._

    val toRightAscension: ValidatedInput[RightAscension] =
      ValidatedInput.requireOne("right ascension",
        microarcseconds.map(Microarcseconds.readLong),
        degrees        .map(Degrees.readDecimal),
        hours          .map(Hours.readDecimal),
        hms            .map(readHms),
        fromLong       .map(_.read),
        fromDecimal    .map(_.read)
      )
  }

  object Input {

    def fromMicroarcseconds(l: Long): Input =
      Input(
        microarcseconds = Some(l),
        degrees         = None,
        hours           = None,
        hms             = None,
        fromLong        = None,
        fromDecimal     = None
      )

    def fromHms(s: String): Input =
      Input(
        microarcseconds = None,
        degrees         = None,
        hours           = None,
        hms             = Some(s),
        fromLong        = None,
        fromDecimal     = None
      )

    implicit val DecoderInput: Decoder[Input] =
      deriveDecoder[Input]

  }

}

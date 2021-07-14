// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import lucuma.odb.api.model.json.target._
import lucuma.core.math.{Angle, HourAngle, RightAscension}
import lucuma.core.optics.SplitMono
import lucuma.core.util.{Display, Enumerated}
import cats.syntax.option._
import cats.syntax.validated._
import io.circe.Decoder
import io.circe.generic.semiauto._

object RightAscensionModel {

  sealed abstract class Units(
    val angleUnit: AngleModel.Units
  ) extends Product with Serializable {

    private def angleToRightAscension[A](m: SplitMono[Angle, A]): SplitMono[RightAscension, A] =
      m.imapA(
        a => RightAscension(HourAngle.fromMicroseconds((a.toMicroarcseconds + 7L)/15L)),
        _.toAngle
      )

    val long: SplitMono[RightAscension, Long] =
      angleToRightAscension(angleUnit.unsignedLong)

    def readLong(value: Long): ValidatedInput[RightAscension] =
      long.reverseGet(value).validNec[InputError]

    val decimal: SplitMono[RightAscension, BigDecimal] =
      angleToRightAscension(angleUnit.unsignedDecimal)

    def readDecimal(value: BigDecimal): ValidatedInput[RightAscension] =
      decimal.reverseGet(value).validNec[InputError]

  }

  object Units {

    case object Microarcseconds extends Units(AngleModel.Units.Microarcseconds)
    case object Degrees         extends Units(AngleModel.Units.Degrees)
    case object Hours           extends Units(AngleModel.Units.Hours)

    val microarcseconds: Units = Microarcseconds
    val degrees: Units         = Degrees
    val hours: Units           = Hours

    implicit val EnumeratedRightAscensionUnits: Enumerated[Units] =
      Enumerated.of(Microarcseconds, Degrees, Hours)

    implicit val DisplayRightAscensionUnits: Display[Units] =
      Display.by(_.angleUnit.abbreviation, _.angleUnit.name)

  }

  implicit val NumericUnitsRightAscension: NumericUnits[RightAscension, Units] =
    NumericUnits.fromRead(_.readLong(_), _.readDecimal(_))

  def readHms(s: String): ValidatedInput[RightAscension] =
    RightAscension
      .fromStringHMS
      .getOption(s)
      .toValidNec(
        InputError.fromMessage(s"Could not parse $s as an HMS string.")
      )

  def writeHms(r: RightAscension): String =
    RightAscension
      .fromStringHMS
      .reverseGet(r)

  final case class Input(
    microarcseconds: Option[Long],
    degrees:         Option[BigDecimal],
    hours:           Option[BigDecimal],
    hms:             Option[RightAscension],
    fromLong:        Option[NumericUnits.LongInput[Units]],
    fromDecimal:     Option[NumericUnits.DecimalInput[Units]]
  ) {

    import Units._

    val toRightAscension: ValidatedInput[RightAscension] =
      ValidatedInput.requireOne("right ascension",
        microarcseconds.map(Microarcseconds.readLong),
        degrees        .map(Degrees.readDecimal),
        hours          .map(Hours.readDecimal),
        hms            .map(_.validNec),
        fromLong       .map(_.read),
        fromDecimal    .map(_.read)
      )
  }

  object Input {

    val Empty: Input =
      Input(None, None, None, None, None, None)

    def fromMicroarcseconds(value: Long): Input =
      Empty.copy(microarcseconds = Some(value))

    def fromDegrees(value: BigDecimal): Input =
      Empty.copy(degrees = Some(value))

    def fromHours(value: BigDecimal): Input =
      Empty.copy(hours = Some(value))

    def fromHms(value: RightAscension): Input =
      Empty.copy(hms = Some(value))

    def fromLong(value: NumericUnits.LongInput[Units]): Input =
      Empty.copy(fromLong = Some(value))

    def fromDecimal(value: NumericUnits.DecimalInput[Units]): Input =
      Empty.copy(fromDecimal = Some(value))

    implicit val DecoderInput: Decoder[Input] =
      deriveDecoder[Input]

    implicit val EqInput: Eq[Input] =
      Eq.by(in => (
        in.microarcseconds,
        in.degrees,
        in.hours,
        in.hms,
        in.fromLong,
        in.fromDecimal
      ))

  }

}

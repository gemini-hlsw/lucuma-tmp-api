// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import lucuma.odb.api.model.json.target._
import lucuma.core.math.{Angle, Declination}
import lucuma.core.optics.SplitMono
import lucuma.core.util.{Display, Enumerated}
import cats.syntax.option._
import cats.syntax.validated._
import io.circe.Decoder
import io.circe.generic.semiauto._
import monocle.Prism


object DeclinationModel {
  sealed abstract class Units(
    val angleUnit: AngleModel.Units
  ) extends Product with Serializable {

    private def fromA[A](m: SplitMono[Angle, A]): Prism[A, Declination] =
      Prism { (a: A) =>
        Declination.fromAngle.getOption(m.reverseGet(a))
      }(d => m.get(d.toAngle))

    val long: Prism[Long, Declination] =
      fromA(angleUnit.signedLong)

    private def range[A](m: SplitMono[Angle, A]): String =
      s"[${m.get(Angle.Angle270)} - ${m.get(Angle.Angle90)}]"

    def readLong(l: Long): ValidatedInput[Declination] =
      long.getOption(l).toValidNec(
        InputError.fromMessage(
          s"Could not read $l ${angleUnit.abbreviation} as Declination in range ${range(angleUnit.signedLong)}"
        )
      )

    val decimal: Prism[BigDecimal, Declination] =
      fromA(angleUnit.signedDecimal)

    def readDecimal(b: BigDecimal): ValidatedInput[Declination] =
      decimal.getOption(b).toValidNec(
        InputError.fromMessage(
          s"Could not read $b ${angleUnit.abbreviation} as Declination in range ${range(angleUnit.signedDecimal)}"
        )
      )

  }

  object Units {

    case object Microarcseconds extends Units(AngleModel.Units.Microarcseconds)
    case object Degrees         extends Units(AngleModel.Units.Degrees)

    val microarcseconds: Units = Microarcseconds
    val degrees: Units         = Degrees

    implicit val EnumeratedDeclinationUnits: Enumerated[Units] =
      Enumerated.of(Microarcseconds, Degrees)

    implicit val DisplayDeclinationUnits: Display[Units] =
      Display.by(_.angleUnit.abbreviation, _.angleUnit.name)

  }

  implicit val NumericUnitsDeclination: NumericUnits[Declination, Units] =
    NumericUnits.fromRead(_.readLong(_), _.readDecimal(_))

  def readDms(s: String): ValidatedInput[Declination] =
    Declination
      .fromStringSignedDMS
      .getOption(s)
      .toValidNec(
        InputError.fromMessage(s"Could not parse $s as a DMS string.")
      )

  def writeDms(d: Declination): String =
    Declination
      .fromStringSignedDMS
      .reverseGet(d)

  final case class Input(
    microarcseconds: Option[Long],
    degrees:         Option[BigDecimal],
    dms:             Option[Declination],
    fromLong:        Option[NumericUnits.LongInput[Units]],
    fromDecimal:     Option[NumericUnits.DecimalInput[Units]]
  ) {

    import Units._

    val toDeclination: ValidatedInput[Declination] =
      ValidatedInput.requireOne("declination",
        microarcseconds.map(Microarcseconds.readLong),
        degrees        .map(Degrees.readDecimal),
        dms            .map(_.validNec),
        fromLong       .map(_.read),
        fromDecimal    .map(_.read)
      )
  }

  object Input {

    val Empty: Input =
      Input(None, None, None, None, None)

    def fromMicroarcseconds(value: Long): Input =
      Empty.copy(microarcseconds = Some(value))

    def fromDegrees(value: BigDecimal): Input =
      Empty.copy(degrees = Some(value))

    def fromDms(value: Declination): Input =
      Empty.copy(dms = Some(value))

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
        in.dms,
        in.fromLong,
        in.fromDecimal
      ))
  }
}

// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.math.Wavelength
import lucuma.core.optics.Format
import lucuma.core.util.{Display, Enumerated}

import cats.Eq
import cats.syntax.option._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder


object WavelengthModel {

  sealed abstract class Units(
    val abbr:      String,
    decimalFormat: Format[BigDecimal, Wavelength]
  ) extends Product with Serializable {

    def readLong(l: Long): ValidatedInput[Wavelength] =
      decimalFormat.getOption(BigDecimal(l)).toValidNec(
        InputError.fromMessage(
          s"Could not read $l $abbr as a wavelength"
        )
      )

    def readDecimal(b: BigDecimal): ValidatedInput[Wavelength] =
      decimalFormat.getOption(b).toValidNec(
        InputError.fromMessage(
          s"Could not read $b $abbr as a wavelength"
        )
      )
  }

  object Units {
    case object Picometers  extends Units("pm", Wavelength.decimalPicometers)
    case object Angstroms   extends Units("Å",  Wavelength.decimalAngstroms)
    case object Nanometers  extends Units("nm", Wavelength.decimalNanometers)
    case object Micrometers extends Units("µm", Wavelength.decimalMicrometers)

    val picometers: Units  = Picometers
    val angstroms: Units   = Angstroms
    val nanometers: Units  = Nanometers
    val micrometers: Units = Micrometers

    implicit val EnumeratedUnits: Enumerated[Units] =
      Enumerated.of(
        Picometers,
        Angstroms,
        Nanometers,
        Micrometers
      )

    implicit val DisplayUnits: Display[Units] =
      Display.byShortName(_.abbr)
  }

  implicit val NumericUnitsWavelength: NumericUnits[Wavelength, Units] =
    NumericUnits.fromRead(_.readLong(_), _.readDecimal(_))

  final case class Input(
    picometers:  Option[Long],
    angstroms:   Option[BigDecimal],
    nanometers:  Option[BigDecimal],
    micrometers: Option[BigDecimal],
    fromLong:    Option[NumericUnits.LongInput[Units]],
    fromDecimal: Option[NumericUnits.DecimalInput[Units]]
  ) {

    import Units._

    def toWavelength(n: String): ValidatedInput[Wavelength] =
      ValidatedInput.requireOne(n,
        picometers .map(Picometers.readLong),
        angstroms  .map(Angstroms.readDecimal),
        nanometers .map(Nanometers.readDecimal),
        micrometers.map(Micrometers.readDecimal),
        fromLong   .map(_.read),
        fromDecimal.map(_.read)
      )

  }

  object Input {

    val Empty: Input =
      Input(None, None, None, None, None, None)

    def fromPicometers(value: Long): Input =
      Empty.copy(picometers = Some(value))

    def fromAngstroms(value: BigDecimal): Input =
      Empty.copy(angstroms = Some(value))

    def fromNanometers(value: BigDecimal): Input =
      Empty.copy(nanometers = Some(value))

    def fromMicrometers(value: BigDecimal): Input =
      Empty.copy(micrometers = Some(value))

    def fromLong(value: NumericUnits.LongInput[Units]): Input =
      Empty.copy(fromLong = Some(value))

    def fromDecimal(value: NumericUnits.DecimalInput[Units]): Input =
      Empty.copy(fromDecimal = Some(value))

    implicit val DecoderInput: Decoder[Input] =
      deriveDecoder[Input]

    implicit val EqInput: Eq[Input] =
      Eq.by(in => (
        in.picometers,
        in.angstroms,
        in.nanometers,
        in.micrometers,
        in.fromLong,
        in.fromDecimal
      ))
  }

}

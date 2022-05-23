// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.math.Wavelength
import lucuma.core.optics.Format
import lucuma.core.util.{Display, Enumerated}
import cats.Eq
import cats.syntax.option._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.all.{PosBigDecimal, PosInt}
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.refined._


object WavelengthModel {

  sealed abstract class Units(
    val abbr:      String,
    decimalFormat: Format[BigDecimal, Wavelength]
  ) extends Product with Serializable {

    def readLong(l: PosInt): ValidatedInput[Wavelength] =
      decimalFormat.getOption(BigDecimal(l.value)).toValidNec(
        InputError.fromMessage(
          s"Could not read $l $abbr as a wavelength"
        )
      )

    def readDecimal(b: PosBigDecimal): ValidatedInput[Wavelength] =
      decimalFormat.getOption(b.value).toValidNec(
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

  final case class WavelengthInput(
    picometers:  Option[PosInt],
    angstroms:   Option[PosBigDecimal],
    nanometers:  Option[PosBigDecimal],
    micrometers: Option[PosBigDecimal]
  ) {

    import Units._

    def toWavelength(n: String): ValidatedInput[Wavelength] =
      ValidatedInput.requireOne(n,
        picometers .map(Picometers.readLong),
        angstroms  .map(Angstroms.readDecimal),
        nanometers .map(Nanometers.readDecimal),
        micrometers.map(Micrometers.readDecimal)
      )

  }

  object WavelengthInput {

    val Empty: WavelengthInput =
      WavelengthInput(None, None, None, None)

    def fromPicometers(value: PosInt): WavelengthInput =
      Empty.copy(picometers = Some(value))

    def fromAngstroms(value: PosBigDecimal): WavelengthInput =
      Empty.copy(angstroms = Some(value))

    def fromNanometers(value: PosBigDecimal): WavelengthInput =
      Empty.copy(nanometers = Some(value))

    def fromMicrometers(value: PosBigDecimal): WavelengthInput =
      Empty.copy(micrometers = Some(value))

    implicit val DecoderInput: Decoder[WavelengthInput] =
      deriveDecoder[WavelengthInput]

    implicit val EqInput: Eq[WavelengthInput] =
      Eq.by(in => (
        in.picometers,
        in.angstroms,
        in.nanometers,
        in.micrometers
      ))
  }

}

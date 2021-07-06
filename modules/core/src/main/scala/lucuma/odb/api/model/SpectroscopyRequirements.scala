// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.syntax.all._
import eu.timepit.refined._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.refined._
import lucuma.odb.api.model.FocalPlane
import lucuma.odb.api.model.SpectroscopyCapabilities
import lucuma.core.math.Angle
import lucuma.core.util.Display
import lucuma.core.math.Wavelength
import lucuma.core.util.Enumerated

final case class SpectroscopyScienceRequirements(
  wavelength:       Option[Wavelength],
  resolution:       Option[PosInt],
  signalToNoise:    Option[PosBigDecimal],
  signalToNoiseAt:  Option[Wavelength],
  wavelengthRange:  Option[Wavelength],
  focalPlane:       Option[FocalPlane],
  focalPlaneAngle:  Option[Angle],
  capabilities:     Option[SpectroscopyCapabilities]
)

object SpectroscopyScienceRequirements {
  val Default = SpectroscopyScienceRequirements(None, refineMV[Positive](1).some, None, None, None, None, None, None)

  implicit val eqSpectroscopyConfigurationOptions: Eq[SpectroscopyScienceRequirements] = Eq.by(x =>
    (x.wavelength,
     x.resolution,
     x.signalToNoise,
     x.signalToNoiseAt,
     x.wavelengthRange,
     x.focalPlane,
     x.focalPlaneAngle,
     x.capabilities
    )
  )
}

object SpectroscopyScienceRequirementsModel {
  final case class Input(
    wavelength:       Option[WavelengthModel.Input],
    resolution:       Option[PosInt],
    signalToNoise:    Option[PosBigDecimal],
    signalToNoiseAt:  Option[WavelengthModel.Input],
    wavelengthRange:  Option[WavelengthModel.Input],
    focalPlane:       Option[FocalPlane],
    focalPlaneAngle:  Option[FocalPlaneAngleInput],
    capabilities:     Option[SpectroscopyCapabilities]
  ) {
    val create: ValidatedInput[SpectroscopyScienceRequirements] =
      (wavelength.traverse(_.toWavelength("wavelength")),
       signalToNoiseAt.traverse(_.toWavelength("signalToNoiseAt")),
       wavelengthRange.traverse(_.toWavelength("wavelengthRange")),
       focalPlaneAngle.traverse(_.toAngle)
      ).mapN { (cw, signalToNoiseAt, wavelengthRange, focalPlaneAngle) =>
        SpectroscopyScienceRequirements(cw, resolution, signalToNoise, signalToNoiseAt, wavelengthRange, focalPlane, focalPlaneAngle, capabilities)
      }
    val edit: ValidatedInput[SpectroscopyScienceRequirements] =
      create
  }

  object Input {
    val Default: Input = Input(None, None, None, None, None, None, None, None)

    implicit val DecoderCreate: Decoder[Input] = deriveDecoder
  }

  final case class FocalPlaneAngleInput(
    microarcseconds: Option[Long],
    milliarcseconds: Option[BigDecimal],
    arcseconds:      Option[BigDecimal]
  ) {
    import Units._

    val toAngle: ValidatedInput[Angle] =
      ValidatedInput.requireOne("focalPlaneAngle",
        microarcseconds.map(Microarcseconds.readLong),
        milliarcseconds.map(Milliarcseconds.readDecimal),
        arcseconds     .map(Arcseconds.readDecimal)
      )
  }

  object FocalPlaneAngleInput {
    implicit def DecoderFocalPlaneAngleInput: Decoder[FocalPlaneAngleInput] =
      deriveDecoder[FocalPlaneAngleInput]

  }

  sealed abstract class Units(
    val angleUnit: AngleModel.Units
  ) extends Product with Serializable {

    def readLong(value: Long): ValidatedInput[Angle] =
      angleUnit.readSignedLong(value)

    def readDecimal(value: BigDecimal): ValidatedInput[Angle] =
      angleUnit.readSignedDecimal(value)
  }

  object Units {

    case object Microarcseconds extends Units(AngleModel.Units.Microarcseconds)
    case object Milliarcseconds extends Units(AngleModel.Units.Milliarcseconds)
    case object Arcseconds      extends Units(AngleModel.Units.Arcseconds)

    val microarcseconds: Units = Microarcseconds
    val milliarcseconds: Units = Milliarcseconds
    val arcseconds: Units      = Arcseconds

    implicit val EnumeratedUnits: Enumerated[Units] =
      Enumerated.of(
        Microarcseconds,
        Milliarcseconds,
        Arcseconds
      )

    implicit def DisplayUnits: Display[Units] =
      Display.byShortName(_.angleUnit.abbreviation)

  }

}

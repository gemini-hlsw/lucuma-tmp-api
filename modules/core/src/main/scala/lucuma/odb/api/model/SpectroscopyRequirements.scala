// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.syntax.all._
import cats.data.State
import clue.data.Input
import eu.timepit.refined._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.refined._
import lucuma.odb.api.model.syntax.input._
import lucuma.core.enum.FocalPlane
import lucuma.core.enum.SpectroscopyCapabilities
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.optics.syntax.lens._
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import monocle.Focus
import monocle.Lens

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

object SpectroscopyScienceRequirements extends SpectroscopyScienceRequirementsOptics {
  val Default: SpectroscopyScienceRequirements =
    SpectroscopyScienceRequirements(None, refineMV[Positive](1).some, None, None, None, None, None, None)

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
  final case class Create(
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
        SpectroscopyScienceRequirements(cw,
                                        resolution,
                                        signalToNoise,
                                        signalToNoiseAt,
                                        wavelengthRange,
                                        focalPlane,
                                        focalPlaneAngle,
                                        capabilities)
      }

  }

  object Create {
    val Default: Create = Create(None, None, None, None, None, None, None, None)

    implicit val DecoderCreate: Decoder[Create] = deriveDecoder

    implicit val EqCreate: Eq[Create] =
      Eq.by { a => (
        a.wavelength,
        a.resolution,
        a.signalToNoise,
        a.signalToNoiseAt,
        a.wavelengthRange,
        a.focalPlane,
        a.focalPlaneAngle,
        a.capabilities
      )}
  }

  final case class Edit(
    wavelength:       Input[WavelengthModel.Input]    = Input.ignore,
    resolution:       Input[PosInt]                   = Input.ignore,
    signalToNoise:    Input[PosBigDecimal]            = Input.ignore,
    signalToNoiseAt:  Input[WavelengthModel.Input]    = Input.ignore,
    wavelengthRange:  Input[WavelengthModel.Input]    = Input.ignore,
    focalPlane:       Input[FocalPlane]               = Input.ignore,
    focalPlaneAngle:  Input[FocalPlaneAngleInput]     = Input.ignore,
    capabilities:     Input[SpectroscopyCapabilities] = Input.ignore
  ) {
    val edit: ValidatedInput[State[SpectroscopyScienceRequirements, Unit]] =
      (wavelength.validateNullable(_.toWavelength("wavelength")),
       signalToNoiseAt.validateNullable(_.toWavelength("signalToNoiseAt")),
       wavelengthRange.validateNullable(_.toWavelength("wavelengthRange")),
       focalPlaneAngle.validateNullable(_.toAngle)
      ).mapN { (cw, signalToNoiseAt, wavelengthRange, focalPlaneAngle) =>
        for {
          _ <- SpectroscopyScienceRequirements.wavelength      := cw
          _ <- SpectroscopyScienceRequirements.resolution      := resolution.toOptionOption
          _ <- SpectroscopyScienceRequirements.signalToNoise   := signalToNoise.toOptionOption
          _ <- SpectroscopyScienceRequirements.signalToNoiseAt := signalToNoiseAt
          _ <- SpectroscopyScienceRequirements.wavelengthRange := wavelengthRange
          _ <- SpectroscopyScienceRequirements.focalPlane      := focalPlane.toOptionOption
          _ <- SpectroscopyScienceRequirements.focalPlaneAngle := focalPlaneAngle
          _ <- SpectroscopyScienceRequirements.capabilities    := capabilities.toOptionOption
        } yield ()
      }
  }

  object Edit {
    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderEdit: Decoder[Edit] = deriveConfiguredDecoder[Edit]
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

    implicit val EqFocalPlaneAngleInput: Eq[FocalPlaneAngleInput] =
      Eq.by { a => (
        a.microarcseconds,
        a.milliarcseconds,
        a.arcseconds
      )}

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

trait SpectroscopyScienceRequirementsOptics {
  /** @group Optics */
  lazy val wavelength: Lens[SpectroscopyScienceRequirements, Option[Wavelength]] =
    Focus[SpectroscopyScienceRequirements](_.wavelength)

  /** @group Optics */
  lazy val resolution: Lens[SpectroscopyScienceRequirements, Option[PosInt]] =
    Focus[SpectroscopyScienceRequirements](_.resolution)

  /** @group Optics */
  lazy val signalToNoise: Lens[SpectroscopyScienceRequirements, Option[PosBigDecimal]] =
    Focus[SpectroscopyScienceRequirements](_.signalToNoise)

  /** @group Optics */
  lazy val signalToNoiseAt: Lens[SpectroscopyScienceRequirements, Option[Wavelength]] =
    Focus[SpectroscopyScienceRequirements](_.signalToNoiseAt)

  /** @group Optics */
  lazy val wavelengthRange: Lens[SpectroscopyScienceRequirements, Option[Wavelength]] =
    Focus[SpectroscopyScienceRequirements](_.wavelengthRange)

  /** @group Optics */
  lazy val focalPlane: Lens[SpectroscopyScienceRequirements, Option[FocalPlane]] =
    Focus[SpectroscopyScienceRequirements](_.focalPlane)

  /** @group Optics */
  lazy val focalPlaneAngle: Lens[SpectroscopyScienceRequirements, Option[Angle]] =
    Focus[SpectroscopyScienceRequirements](_.focalPlaneAngle)

  /** @group Optics */
  lazy val capabilities: Lens[SpectroscopyScienceRequirements, Option[SpectroscopyCapabilities]] =
    Focus[SpectroscopyScienceRequirements](_.capabilities)
}

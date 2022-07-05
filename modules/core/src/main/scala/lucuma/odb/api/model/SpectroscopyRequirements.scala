// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.syntax.all._
import cats.data.StateT
import clue.data.Input
import eu.timepit.refined._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.refined._
import lucuma.core.enums.FocalPlane
import lucuma.core.enums.SpectroscopyCapabilities
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.odb.api.model.WavelengthModel.WavelengthInput
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.lens._
import lucuma.odb.api.model.syntax.validatedinput._
import monocle.Focus
import monocle.Lens

final case class SpectroscopyScienceRequirements(
  wavelength:         Option[Wavelength],
  resolution:         Option[PosInt],
  signalToNoise:      Option[PosBigDecimal],
  signalToNoiseAt:    Option[Wavelength],
  wavelengthCoverage: Option[Wavelength],
  focalPlane:         Option[FocalPlane],
  focalPlaneAngle:    Option[Angle],
  capabilities:       Option[SpectroscopyCapabilities]
) {

  def exposureTimeMode: Option[ExposureTimeMode] =
    signalToNoise.map(ExposureTimeMode.SignalToNoise(_))

}

object SpectroscopyScienceRequirements extends SpectroscopyScienceRequirementsOptics {
  val Default: SpectroscopyScienceRequirements =
    SpectroscopyScienceRequirements(None, refineMV[Positive](1).some, None, None, None, None, None, None)

  implicit val eqSpectroscopyConfigurationOptions: Eq[SpectroscopyScienceRequirements] = Eq.by(x =>
    (x.wavelength,
     x.resolution,
     x.signalToNoise,
     x.signalToNoiseAt,
     x.wavelengthCoverage,
     x.focalPlane,
     x.focalPlaneAngle,
     x.capabilities
    )
  )
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
  lazy val wavelengthCoverage: Lens[SpectroscopyScienceRequirements, Option[Wavelength]] =
    Focus[SpectroscopyScienceRequirements](_.wavelengthCoverage)

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

final case class FocalPlaneAngleInput(
  microarcseconds: Option[Long],
  milliarcseconds: Option[BigDecimal],
  arcseconds:      Option[BigDecimal]
) extends EditorInput[Angle] {
  import FocalPlaneAngleInput.Units._

  override val create: ValidatedInput[Angle] =
    ValidatedInput.requireOne("focalPlaneAngle",
      microarcseconds.map(Microarcseconds.readLong),
      milliarcseconds.map(Milliarcseconds.readDecimal),
      arcseconds     .map(Arcseconds.readDecimal)
    )

  override val edit: StateT[EitherInput, Angle, Unit] =
    StateT.setF[EitherInput, Angle](create.toEither)
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

final case class SpectroscopyScienceRequirementsInput (
  wavelength:         Input[WavelengthInput]          = Input.ignore,
  resolution:         Input[PosInt]                   = Input.ignore,
  signalToNoise:      Input[PosBigDecimal]            = Input.ignore,
  signalToNoiseAt:    Input[WavelengthInput]          = Input.ignore,
  wavelengthCoverage: Input[WavelengthInput]          = Input.ignore,
  focalPlane:         Input[FocalPlane]               = Input.ignore,
  focalPlaneAngle:    Input[FocalPlaneAngleInput]     = Input.ignore,
  capabilities:       Input[SpectroscopyCapabilities] = Input.ignore
) extends EditorInput[SpectroscopyScienceRequirements] {

  override val create: ValidatedInput[SpectroscopyScienceRequirements] =
    (wavelength.toOption.traverse(_.toWavelength("wavelength")),
     signalToNoiseAt.toOption.traverse(_.toWavelength("signalToNoiseAt")),
     wavelengthCoverage.toOption.traverse(_.toWavelength("wavelengthCoverage")),
     focalPlaneAngle.toOption.traverse(_.create)
    ).mapN { (cw, signalToNoiseAt, wavelengthCoverage, focalPlaneAngle) =>
      SpectroscopyScienceRequirements(cw,
                                      resolution.toOption,
                                      signalToNoise.toOption,
                                      signalToNoiseAt,
                                      wavelengthCoverage,
                                      focalPlane.toOption,
                                      focalPlaneAngle,
                                      capabilities.toOption)
    }

  override val edit: StateT[EitherInput, SpectroscopyScienceRequirements, Unit] = {
    val validArgs =
      (wavelength.validateNullable(_.toWavelength("wavelength")),
       signalToNoiseAt.validateNullable(_.toWavelength("signalToNoiseAt")),
       wavelengthCoverage.validateNullable(_.toWavelength("wavelengthCoverage")),
       focalPlaneAngle.validateNullable(_.create)
      ).tupled

    for {
      args <- validArgs.liftState
      (cw, signalToNoiseAt, wavelengthCoverage, focalPlaneAngle) = args
      _ <- SpectroscopyScienceRequirements.wavelength         := cw
      _ <- SpectroscopyScienceRequirements.resolution         := resolution.toOptionOption
      _ <- SpectroscopyScienceRequirements.signalToNoise      := signalToNoise.toOptionOption
      _ <- SpectroscopyScienceRequirements.signalToNoiseAt    := signalToNoiseAt
      _ <- SpectroscopyScienceRequirements.wavelengthCoverage := wavelengthCoverage
      _ <- SpectroscopyScienceRequirements.focalPlane         := focalPlane.toOptionOption
      _ <- SpectroscopyScienceRequirements.focalPlaneAngle    := focalPlaneAngle
      _ <- SpectroscopyScienceRequirements.capabilities       := capabilities.toOptionOption
    } yield ()
  }
}

object SpectroscopyScienceRequirementsInput {
  import io.circe.generic.extras.semiauto._
  import io.circe.generic.extras.Configuration
  implicit val customConfig: Configuration = Configuration.default.withDefaults

  val Default: SpectroscopyScienceRequirementsInput =
    SpectroscopyScienceRequirementsInput(resolution = Input.assign(refineMV[Positive](1)))

  implicit val DecoderSpectroscopyScienceRequirementsInput: Decoder[SpectroscopyScienceRequirementsInput] =
    deriveConfiguredDecoder[SpectroscopyScienceRequirementsInput]

  implicit val EqSpectroscopyScienceRequirementsInput: Eq[SpectroscopyScienceRequirementsInput] =
    Eq.by { a => (
      a.wavelength,
      a.resolution,
      a.signalToNoise,
      a.signalToNoiseAt,
      a.wavelengthCoverage,
      a.focalPlane,
      a.focalPlaneAngle,
      a.capabilities
    )}
}

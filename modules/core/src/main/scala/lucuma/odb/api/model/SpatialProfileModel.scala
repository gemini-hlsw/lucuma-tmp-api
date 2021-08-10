// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.syntax.all._
import io.circe.Decoder
import io.circe.generic.semiauto._
import lucuma.core.enum.SpatialProfileType
import lucuma.core.util.Enumerated
import lucuma.core.util.Display
import lucuma.core.math.Angle

object SpatialProfileModel {
  final case class Input(sourceType: SpatialProfileType, fwhm: Option[GaussianSourceAngleInput]) extends Product with Serializable {
    def toSpatialProfile: ValidatedInput[SpatialProfile] = sourceType match {
      case SpatialProfileType.PointSource => SpatialProfile.PointSource.validNec
      case SpatialProfileType.UniformSource => SpatialProfile.UniformSource.validNec
      case SpatialProfileType.GaussianSource =>
        fwhm.map {
          _.toAngle.map(SpatialProfile.GaussianSource)
        }.getOrElse(InputError.fromMessage("GaussianSource requires a valid fwhm angle").invalidNec[SpatialProfile])
    }
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

  final case class GaussianSourceAngleInput(
    microarcseconds: Option[Long],
    milliarcseconds: Option[BigDecimal],
    arcseconds:      Option[BigDecimal],
  ) {
    import Units._
    val toAngle: ValidatedInput[Angle] =
      ValidatedInput.requireOne("fwhm",
        microarcseconds.map(Microarcseconds.readLong),
        milliarcseconds.map(Milliarcseconds.readDecimal),
        arcseconds     .map(Arcseconds.readDecimal)
      )
  }

  object GaussianSourceAngleInput {
    implicit def DecoderGaussianSourceAngleInput: Decoder[GaussianSourceAngleInput] =
      deriveDecoder[GaussianSourceAngleInput]

  }

  object Input {

    implicit val DecoderInput: Decoder[Input] =
      deriveDecoder[Input]

  }
}


// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import io.circe.Decoder
import io.circe.generic.semiauto._
import lucuma.core.util.Enumerated
import lucuma.core.util.Display

object SpatialProfileModel {
  final case class Input(sourceType: SpatialProfileType, fwhm: Option[GaussianSourceAngleInput]) extends Product with Serializable

  sealed abstract class Units(
    val angleUnit: AngleModel.Units
  ) extends Product with Serializable

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
    fromLong:        Option[NumericUnits.LongInput[Units]],
    fromDecimal:     Option[NumericUnits.DecimalInput[Units]]
  )

  object GaussianSourceAngleInput {
    implicit def DecoderGaussianSourceAngleInput: Decoder[GaussianSourceAngleInput] =
      deriveDecoder[GaussianSourceAngleInput]

  }

  sealed trait SpatialProfileType extends Product with Serializable

  final case object PointSource    extends SpatialProfileType
  final case object UniformSource  extends SpatialProfileType
  final case object GaussianSource extends SpatialProfileType

  object SpatialProfileType {

    implicit val EnumeratedProfileType: Enumerated[SpatialProfileType] =
      Enumerated.of(
        PointSource,
        UniformSource,
        GaussianSource
      )

    implicit val DecoderSpatialProfileType: Decoder[SpatialProfileType] =
      deriveDecoder[SpatialProfileType]

  }

  object Input {

    implicit val DecoderInput: Decoder[Input] =
      deriveDecoder[Input]

  }
}


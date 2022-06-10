// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.gmos.longslit

import cats.Eq
import cats.data.{NonEmptyList, StateT}
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.traverse._
import clue.data.Input
import coulomb.Quantity
import eu.timepit.refined.cats._
import eu.timepit.refined.types.all.NonEmptyString
import io.circe.Decoder
import io.circe.refined._
import lucuma.core.`enum`.{GmosAmpGain, GmosAmpReadMode, GmosRoi, GmosXBinning, GmosYBinning}
import lucuma.core.math.Axis.Q
import lucuma.core.math.{Offset, Wavelength}
import lucuma.core.math.units.Nanometer
import lucuma.odb.api.model.{EditorInput, EitherInput, InputError, OffsetModel, ValidatedInput}
import lucuma.odb.api.model.ExposureTimeMode.ExposureModeInput
import lucuma.odb.api.model.WavelengthModel.WavelengthInput
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.lens._


final case class AdvancedConfigInput[G, F, U](
  name:                        Input[NonEmptyString]                   = Input.ignore,
  overrideWavelength:          Input[WavelengthInput]                  = Input.ignore,
  overrideGrating:             Input[G]                                = Input.ignore,
  overrideFilter:              Input[Option[F]]                        = Input.ignore,
  overrideFpu:                 Input[U]                                = Input.ignore,
  overrideExposureTimeMode:    Input[ExposureModeInput]                = Input.ignore,
  explicitXBin:                Input[GmosXBinning]                     = Input.ignore,
  explicitYBin:                Input[GmosYBinning]                     = Input.ignore,
  explicitAmpReadMode:         Input[GmosAmpReadMode]                  = Input.ignore,
  explicitAmpGain:             Input[GmosAmpGain]                      = Input.ignore,
  explicitRoi:                 Input[GmosRoi]                          = Input.ignore,
  explicitWavelengthDithersNm: Input[List[BigDecimal]]                 = Input.ignore,
  explicitSpatialOffsets:      Input[List[OffsetModel.ComponentInput]] = Input.ignore
) extends EditorInput[AdvancedConfig[G, F, U]] {

  override val create: ValidatedInput[AdvancedConfig[G, F, U]] =
    (explicitSpatialOffsets.toOption.toList.flatten.traverse(_.toComponent[Q]),
     overrideWavelength.toOption.traverse(_.toWavelength("overrideWavelength")),
     overrideExposureTimeMode.toOption.traverse(_.create)
    ).mapN { (os, wavelength, exp) =>
      AdvancedConfig(
        name.toOption,
        overrideWavelength       = wavelength,
        overrideGrating          = overrideGrating.toOption,
        overrideFilter           = overrideFilter.toOption,
        overrideFpu              = overrideFpu.toOption,
        overrideExposureTimeMode = exp,
        explicitXBin             = explicitXBin.toOption,
        explicitYBin             = explicitYBin.toOption,
        explicitAmpReadMode      = explicitAmpReadMode.toOption,
        explicitAmpGain          = explicitAmpGain.toOption,
        explicitRoi              = explicitRoi.toOption,
        explicitλDithers         = NonEmptyList.fromList(explicitWavelengthDithersNm.toOption.toList.flatten.map(d => Quantity[BigDecimal, Nanometer](d))),
        explicitSpatialOffsets   = NonEmptyList.fromList(os)
      )
    }

  override val edit: StateT[EitherInput, AdvancedConfig[G, F, U], Unit] =
    for {
      _ <- AdvancedConfig.name                          := name.toOptionOption
      _ <- AdvancedConfig.overrideWavelength            :<
        overrideWavelength.fold(
          StateT.empty[EitherInput, Option[Wavelength], Unit],
          StateT.setF(Option.empty[Wavelength].rightNec[InputError]),
          in => StateT.setF(in.toWavelength("overrideWavelength").toEither.map(_.some))
        ).some
      _ <- AdvancedConfig.overrideGrating[G, F, U]      := overrideGrating.toOptionOption
      _ <- AdvancedConfig.overrideFilter[G, F, U]       := overrideFilter.toOptionOption
      _ <- AdvancedConfig.overrideFpu[G, F, U]          := overrideFpu.toOptionOption
      _ <- AdvancedConfig.overrideExposureMode[G, F, U] :? overrideExposureTimeMode
      _ <- AdvancedConfig.explicitXBin                  := explicitXBin.toOptionOption
      _ <- AdvancedConfig.explicitYBin                  := explicitYBin.toOptionOption
      _ <- AdvancedConfig.explicitAmpReadMode           := explicitAmpReadMode.toOptionOption
      _ <- AdvancedConfig.explicitAmpGain               := explicitAmpGain.toOptionOption
      _ <- AdvancedConfig.explicitRoi                   := explicitRoi.toOptionOption
      _ <- AdvancedConfig.explicitλDithers              :<
        explicitWavelengthDithersNm.fold(
          StateT.empty[EitherInput, Option[NonEmptyList[Quantity[BigDecimal, Nanometer]]], Unit],
          StateT.setF(Option.empty[NonEmptyList[Quantity[BigDecimal, Nanometer]]].rightNec[InputError]),
          deltas => StateT.setF(
            NonEmptyList.fromList(deltas.map(d => Quantity[BigDecimal, Nanometer](d))).rightNec[InputError]
          )
        ).some
      _ <- AdvancedConfig.explicitSpatialOffsets       :<
        explicitSpatialOffsets.fold(
          StateT.empty[EitherInput, Option[NonEmptyList[Offset.Q]], Unit],
          StateT.setF(Option.empty[NonEmptyList[Offset.Q]].rightNec[InputError]),
          os => StateT.setF(
            os.traverse(_.toComponent[Q])
              .map(NonEmptyList.fromList)
              .toEither
          )
        ).some
    } yield ()

}

object AdvancedConfigInput {

  import io.circe.generic.extras.semiauto._
  import io.circe.generic.extras.Configuration
  implicit val customConfig: Configuration = Configuration.default.withDefaults

  implicit def DecoderAdvancedConfigInput[G: Decoder, F: Decoder, U: Decoder]: Decoder[AdvancedConfigInput[G, F, U]] =
    deriveConfiguredDecoder[AdvancedConfigInput[G, F, U]]

  implicit def EqAdvancedConfigInput[G: Eq, F: Eq, U: Eq]: Eq[AdvancedConfigInput[G, F, U]] =
    Eq.by { a => (
      a.name,
      a.overrideWavelength,
      a.overrideGrating,
      a.overrideFilter,
      a.overrideFpu,
      a.overrideExposureTimeMode,
      a.explicitXBin,
      a.explicitYBin,
      a.explicitAmpReadMode,
      a.explicitAmpGain,
      a.explicitRoi,
      a.explicitWavelengthDithersNm,
      a.explicitSpatialOffsets
    )}

}
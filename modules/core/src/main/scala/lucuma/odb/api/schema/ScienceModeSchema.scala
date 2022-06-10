// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.syntax.option._
import lucuma.core.math.Axis.Q
import lucuma.core.syntax.string._
import lucuma.odb.api.model.{ConfigurationMode, ExposureTimeMode, ScienceMode}
import lucuma.odb.api.model.gmos.longslit.{AdvancedConfig, BasicConfig}
import lucuma.odb.api.schema.syntax.all._
import sangria.schema._

object ScienceModeSchema {

  import GmosSchema._
  import InstrumentSchema.EnumTypeInstrument
  import TimeSchema.NonNegativeDurationType
  import RefinedSchema._

  implicit val EnumTypeConfigurationMode: EnumType[ConfigurationMode] =
    EnumType.fromEnumerated(
      "ConfigurationModeType",
      "ConfigurationMode"
    )

  def SignalToNoiseExposureModeType: ObjectType[Any, ExposureTimeMode.SignalToNoise] =
    ObjectType[Any, ExposureTimeMode.SignalToNoise](
      name        = "SignalToNoiseMode",
      description = "Signal to noise exposure time mode",
      fields[Any, ExposureTimeMode.SignalToNoise](
        Field(
          name        = "value",
          fieldType   = PosBigDecimalType,
          description = "Signal/Noise value".some,
          resolve     = _.value.value
        )
      )
    )

  def FixedExposureModeType: ObjectType[Any, ExposureTimeMode.FixedExposure] =
    ObjectType[Any, ExposureTimeMode.FixedExposure](
      name        = "FixedExposureMode",
      description = "Fixed exposure time mode",
      fields[Any, ExposureTimeMode.FixedExposure](
        Field(
          name        = "count",
          fieldType   = NonNegIntType,
          description = "Exposure count".some,
          resolve     = _.value.count
        ),

        Field(
          name        = "time",
          fieldType   = NonNegativeDurationType,
          description = "Exposure time".some,
          resolve     = _.value.time
        )
      )
    )

  def ExposureTimeModeType: ObjectType[Any, ExposureTimeMode] =
    ObjectType[Any, ExposureTimeMode](
      name        = "ExposureTimeMode",
      description = "Exposure time mode, either signal to noise or fixed",
      fields[Any, ExposureTimeMode](
        Field(
          name        = "signalToNoise",
          fieldType   = OptionType(SignalToNoiseExposureModeType),
          description = "Signal to noise exposure time mode data, if applicable".some,
          resolve     = c => ExposureTimeMode.signalToNoise.getOption(c.value)
        ),

        Field(
          name        = "fixedExposure",
          fieldType   = OptionType(FixedExposureModeType),
          description = "Fixed exposure time mode data, if applicable".some,
          resolve     = c => ExposureTimeMode.fixedExposure.getOption(c.value)
        )
      )
    )

  def ScienceModeType: ObjectType[Any, ScienceMode] =
    ObjectType[Any, ScienceMode](
      name         = "ScienceMode",
      description  = "Base science mode",
      fields[Any, ScienceMode](

        Field(
          name        = "instrument",
          fieldType   = EnumTypeInstrument,
          description = "Instrument".some,
          resolve     = _.value.instrument
        ),

        Field(
          name        = "mode",
          fieldType   = EnumTypeConfigurationMode,
          description = "Mode type".some,
          resolve     = _.value.mode
        ),

        Field(
          name        = "gmosNorthLongSlit",
          fieldType   = OptionType(GmosNLongSlitType),
          description = "GMOS North Long Slit mode".some,
          resolve     = _.value.fold(_.some, _ => Option.empty)
        ),

        Field(
          name        = "gmosSouthLongSlit",
          fieldType   = OptionType(GmosSLongSlitType),
          description = "GMOS South Long Slit mode".some,
          resolve     = _.value.fold(_ => Option.empty, _.some)
        )
      )
    )

  def gmosLongSlitBasicType[G, F, U](
    siteName:    String,
    gratingEnum: EnumType[G],
    filterEnum:  EnumType[F],
    fpuEnum:     EnumType[U]
  ): ObjectType[Any, BasicConfig[G, F, U]] =
    ObjectType[Any, BasicConfig[G, F, U]](
      name        = s"Gmos${siteName.capitalize}LongSlitBasic",
      description = s"GMOS ${siteName.capitalize} Long Slit basic configuration",

      fields[Any, BasicConfig[G, F, U]](

        Field(
          name        = "grating",
          fieldType   = gratingEnum,
          description = "GMOS North Grating".some,
          resolve     = _.value.grating
        ),

        Field(
          name        = "filter",
          fieldType   = OptionType(filterEnum),
          description = "GMOS North Filter".some,
          resolve     = _.value.filter
        ),

        Field(
          name        = "fpu",
          fieldType   = fpuEnum,
          description = "GMOS North FPU".some,
          resolve     = _.value.fpu
        )
      )
    )

  def gmosLongSlitAdvancedType[G, F, U](
    siteName:    String,
    gratingEnum: EnumType[G],
    filterEnum:  EnumType[F],
    fpuEnum:     EnumType[U]
  ): ObjectType[Any, AdvancedConfig[G, F, U]] =
    ObjectType[Any, AdvancedConfig[G, F, U]](
      name        = s"Gmos${siteName.capitalize}LongSlitAdvanced",
      description = s"GMOS ${siteName.capitalize} Long Slit advanced configuration",

      fields[Any, AdvancedConfig[G, F, U]](

        Field(
          name        = "overrideWavelength",
          fieldType   = OptionType(WavelengthSchema.WavelengthType),
          description = "Overrides the science requirement wavelength".some,
          resolve     = _.value.overrideWavelength
        ),

        Field(
          name        = "overrideGrating",
          fieldType   = OptionType(gratingEnum),
          description = "GMOS North Grating override, taking the place of the basic configuration grating".some,
          resolve     = _.value.overrideGrating
        ),

        Field(
          name        = "overrideFilter",
          fieldType   = OptionType(OptionType(filterEnum)),
          description = "GMOS North filter override, taking the place of the basic configuration filter".some,
          resolve     = _.value.overrideFilter
        ),

        Field(
          name        = "overrideFpu",
          fieldType   = OptionType(fpuEnum),
          description = "GMOS North FPU override, taking the place of the basic configuration FPU".some,
          resolve     = _.value.overrideFpu
        ),

        Field(
          name        = "overrideExposureTimeMode",
          fieldType   = OptionType(ExposureTimeModeType),
          description = "Exposure time mode, taking the place of the value from science requirements".some,
          resolve     = _.value.overrideExposureTimeMode
        ),

        Field(
          name        = "explicitXBin",
          fieldType   = OptionType(EnumTypeGmosXBinning),
          description = "Explicitly specified GMOS X-Binning, override the default (calculated from effective slit size)".some,
          resolve     = _.value.explicitXBin
        ),

        Field(
          name        = "explicitYBin",
          fieldType   = OptionType(EnumTypeGmosYBinning),
          description = s"Explicitly specified GMOS Y-Binning, override the default (${AdvancedConfig.DefaultYBinning.tag.toScreamingSnakeCase})".some,
          resolve     = _.value.explicitYBin
        ),

        Field(
          name        = "explicitAmpReadMode",
          fieldType   = OptionType(EnumTypeGmosAmpReadMode),
          description = s"Explicitly specified GMOS amp read mode, override the default (${AdvancedConfig.DefaultAmpReadMode.tag.toScreamingSnakeCase})".some,
          resolve     = _.value.explicitAmpReadMode
        ),

        Field(
          name        = "explicitAmpGain",
          fieldType   = OptionType(EnumTypeGmosAmpGain),
          description = s"Explicitly specified GMOS amp gain, override the default (${AdvancedConfig.DefaultAmpGain.tag.toScreamingSnakeCase})".some,
          resolve     = _.value.explicitAmpGain
        ),

        Field(
          name        = "explicitRoi",
          fieldType   = OptionType(EnumTypeGmosRoi),
          description = s"Explicitly specified GMOS ROI, overriding the default (${AdvancedConfig.DefaultRoi.tag.toScreamingSnakeCase})".some,
          resolve     = _.value.explicitRoi
        ),

        Field(
          name        = "explicitWavelengthDithersNm",
          fieldType   = OptionType(ListType(BigDecimalType)),
          description = s"Explicitly specified wavelength dithers required to fill in the chip gaps (in nm), taking the place of the calculated value based on the grating dispersion".some,
          resolve     = _.value.explicitλDithers.map(_.toList.map(_.value))
        ),

        Field(
          name        = "explicitSpatialOffsets",
          fieldType   = OptionType(ListType(OffsetSchema.OffsetComponentType[Q]("q"))),
          description = s"Explicitly specified spatial q offsets, overriding the default".some,
          resolve     = _.value.explicitSpatialOffsets.map(_.toList)
        )
      )
    )


  val GmosNLongSlitType: ObjectType[Any, ScienceMode.GmosNorthLongSlit] =
    ObjectType[Any, ScienceMode.GmosNorthLongSlit](
      name        = "GmosNorthLongSlit",
      description = "GMOS North Long Slit mode",

      fields[Any, ScienceMode.GmosNorthLongSlit](
        Field(
          name        = "basic",
          fieldType   = gmosLongSlitBasicType("north", EnumTypeGmosNorthGrating, EnumTypeGmosNorthFilter, EnumTypeGmosNorthFpu),
          description = "GMOS North Long Slit basic configuration".some,
          resolve     = _.value.basic
        ),

        Field(
          name        = "advanced",
          fieldType   = OptionType(gmosLongSlitAdvancedType("north", EnumTypeGmosNorthGrating, EnumTypeGmosNorthFilter, EnumTypeGmosNorthFpu)),
          description = "GMOS North Long Slit advanced configuration".some,
          resolve     = _.value.advanced
        )
      )
    )

  val GmosSLongSlitType: ObjectType[Any, ScienceMode.GmosSouthLongSlit] =
    ObjectType[Any, ScienceMode.GmosSouthLongSlit](
      name        = "GmosSouthLongSlit",
      description = "GMOS South Long Slit mode",

      fields[Any, ScienceMode.GmosSouthLongSlit](
        Field(
          name        = "basic",
          fieldType   = gmosLongSlitBasicType("South", EnumTypeGmosSouthGrating, EnumTypeGmosSouthFilter, EnumTypeGmosSouthFpu),
          description = "GMOS South Long Slit basic configuration".some,
          resolve     = _.value.basic
        ),

        Field(
          name        = "advanced",
          fieldType   = OptionType(gmosLongSlitAdvancedType("South", EnumTypeGmosSouthGrating, EnumTypeGmosSouthFilter, EnumTypeGmosSouthFpu)),
          description = "GMOS South Long Slit advanced configuration".some,
          resolve     = _.value.advanced
        )
      )
    )

}

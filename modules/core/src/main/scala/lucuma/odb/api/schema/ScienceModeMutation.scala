// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.`enum`.{GmosNorthFilter, GmosNorthFpu, GmosNorthGrating, GmosSouthFilter, GmosSouthFpu, GmosSouthGrating}
import lucuma.odb.api.model.gmos.longslit.{AdvancedConfigInput, BasicConfigInput}
import lucuma.odb.api.model.{ExposureTimeMode, ScienceMode, ScienceModeInput}
import lucuma.odb.api.model.ExposureTimeMode.{FixedExposureInput, SignalToNoiseInput}
import sangria.schema._

trait ScienceModeMutation {

  import syntax.inputtype._
  import GmosSchema._
  import RefinedSchema._
  import TimeSchema.InputObjectTypeNonNegDuration
  import WavelengthSchema.InputWavelength

  implicit val InputObjectTypeSignalToNoise: InputObjectType[SignalToNoiseInput] =
    InputObjectType[SignalToNoiseInput](
      "SignalToNoiseModeInput",
      "Signal-to-noise mode parameters",
      List(
        InputField("value", PosBigDecimalType, "s/n value")
      )
    )

  implicit val InputObjectTypeFixedExposureMode: InputObjectType[FixedExposureInput] =
    InputObjectType[FixedExposureInput](
      "FixedExposureModeInput",
      "Fixed exposure time mode parameters",
      List(
        InputField("count", NonNegIntType,                 "exposure count"),
        InputField("time",  InputObjectTypeNonNegDuration, "exposure time")
      )
    )

  implicit val InputObjectTypeExposureTimeMode: InputObjectType[ExposureTimeMode] =
    InputObjectType[ExposureTimeMode](
      "ExposureTimeModeInput",
      "Exposure time mode input.  Specify fixed or signal to noise, but not both",
      List(
        InputObjectTypeSignalToNoise.notNullableField("signalToNoise"),
        InputObjectTypeFixedExposureMode.notNullableField("fixedExposure")
      )
    )

  def inputObjectTypeGmosLongSlitBasicConfig[G, F, U](
    siteName:    String,
    gratingEnum: EnumType[G],
    filterEnum:  EnumType[F],
    fpuEnum:     EnumType[U]
  ): InputObjectType[BasicConfigInput[G, F, U]] =
    InputObjectType[BasicConfigInput[G, F, U]](
      s"Gmos${siteName.capitalize}LongSlitBasicConfigInput",
      s"Edit or create GMOS ${siteName.capitalize} Long Slit basic configuration",
      List(
        gratingEnum.notNullableField("grating"),
        filterEnum.nullableField("filter"),
        fpuEnum.notNullableField("fpu")
      )
    )

  implicit val InputObjectTypeGmosNorthLongSlitBasicConfig: InputObjectType[BasicConfigInput[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]] =
    inputObjectTypeGmosLongSlitBasicConfig(
      "north",
      EnumTypeGmosNorthGrating,
      EnumTypeGmosNorthFilter,
      EnumTypeGmosNorthFpu
    )

  implicit val InputObjectTypeGmosSouthLongSlitBasicConfig: InputObjectType[BasicConfigInput[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]] =
    inputObjectTypeGmosLongSlitBasicConfig(
      "south",
      EnumTypeGmosSouthGrating,
      EnumTypeGmosSouthFilter,
      EnumTypeGmosSouthFpu
    )

  def inputObjectTypeGmosLongSlitAdvancedConfig[G, F, U](
    siteName:    String,
    gratingEnum: EnumType[G],
    filterEnum:  EnumType[F],
    fpuEnum:     EnumType[U]
  ): InputObjectType[AdvancedConfigInput[G, F, U]] =
    InputObjectType[AdvancedConfigInput[G, F, U]](
      s"Gmos${siteName.capitalize}LongSlitAdvancedConfigInput",
      s"Edit or create GMOS ${siteName.capitalize} Long Slit advanced configuration",
      List(
        InputWavelength.nullableField("overrideWavelength"),
        gratingEnum.nullableField("overrideGrating"),
        filterEnum.nullableField("overrideFilter"),
        fpuEnum.nullableField("overrideFpu"),
        InputObjectTypeExposureTimeMode.nullableField("overrideExposureTimeMode"),
        EnumTypeGmosXBinning.nullableField("explicitXBin"),
        EnumTypeGmosYBinning.nullableField("explicitYBin"),
        EnumTypeGmosAmpReadMode.nullableField("explicitAmpReadMode"),
        EnumTypeGmosAmpGain.nullableField("explicitAmpGain"),
        EnumTypeGmosRoi.nullableField("explicitRoi"),
        ListInputType(BigDecimalType).nullableField("explicitWavelengthDithersNm"),
        ListInputType(OffsetSchema.InputObjectTypeOffsetComponentInput).nullableField("explicitSpatialOffsets")
      )
    )

  implicit val InputObjectTypeGmosNorthLongSlitAdvancedConfig: InputObjectType[AdvancedConfigInput[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]] =
    inputObjectTypeGmosLongSlitAdvancedConfig(
      "north",
      EnumTypeGmosNorthGrating,
      EnumTypeGmosNorthFilter,
      EnumTypeGmosNorthFpu
    )

  implicit val InputObjectTypeGmosSouthLongSlitAdvancedConfig: InputObjectType[AdvancedConfigInput[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]] =
    inputObjectTypeGmosLongSlitAdvancedConfig(
      "south",
      EnumTypeGmosSouthGrating,
      EnumTypeGmosSouthFilter,
      EnumTypeGmosSouthFpu
    )

  implicit val InputObjectTypeGmosNorthLongSlit: InputObjectType[ScienceMode.GmosNorthLongSlitInput] =
    InputObjectType[ScienceMode.GmosNorthLongSlitInput](
      "GmosNorthLongSlitInput",
      "Edit or create GMOS North Long Slit configuration",
      List(
        InputObjectTypeGmosNorthLongSlitBasicConfig.notNullableField("basic"),
        InputObjectTypeGmosNorthLongSlitAdvancedConfig.nullableField("advanced")
      )
    )

  implicit val InputObjectTypeGmosSouthLongSlit: InputObjectType[ScienceMode.GmosSouthLongSlitInput] =
    InputObjectType[ScienceMode.GmosSouthLongSlitInput](
      "GmosSouthLongSlitInput",
      "Edit or create GMOS South Long Slit configuration",
      List(
        InputObjectTypeGmosSouthLongSlitBasicConfig.notNullableField("basic"),
        InputObjectTypeGmosSouthLongSlitAdvancedConfig.nullableField("advanced")
      )
    )

  implicit val InputObjectTypeScienceMode: InputObjectType[ScienceModeInput] =
    InputObjectType[ScienceModeInput](
      "ScienceModeInput",
      "Edit or create an observation's science mode",
      List(
        InputObjectTypeGmosNorthLongSlit.notNullableField("gmosNorthLongSlit"),
        InputObjectTypeGmosSouthLongSlit.notNullableField("gmosSouthLongSlit")
      )
    )

}

object ScienceModeMutation extends ScienceModeMutation

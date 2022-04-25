// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.syntax.option._

import lucuma.odb.api.model.ConfigurationMode
import lucuma.odb.api.model.ScienceConfigurationModel
import lucuma.odb.api.model.ScienceConfigurationModel.Modes
import lucuma.odb.api.schema.syntax.all._

import sangria.schema._

object ScienceConfigurationSchema {
  import GmosSchema._
  import InstrumentSchema.EnumTypeInstrument

  implicit val EnumTypeConfigurationMode: EnumType[ConfigurationMode] =
    EnumType.fromEnumerated(
      "ConfigurationModeType",
      "ConfigurationMode"
    )

  def ScienceConfigurationType: ObjectType[Any, ScienceConfigurationModel] =
    ObjectType[Any, ScienceConfigurationModel](
      name         = "ScienceConfiguration",
      description  = "Base science configuration",
      fields[Any, ScienceConfigurationModel](

        Field(
          name        = "instrument",
          fieldType   = EnumTypeInstrument,
          description = "Instrument".some,
          resolve     = _.value.instrument
        ),

        Field(
          name        = "mode",
          fieldType   = EnumTypeConfigurationMode,
          description = "Configuration mode".some,
          resolve     = _.value.mode
        ),

        Field(
          name        = "gmosNorthLongSlit",
          fieldType   = OptionType(GmosNLongSlitType),
          description = "GMOS North Long Slit configuration".some,
          resolve     = _.value.fold(_.some, _ => Option.empty)
        ),

        Field(
          name        = "gmosSouthLongSlit",
          fieldType   = OptionType(GmosSLongSlitType),
          description = "GMOS South Long Slit configuration".some,
          resolve     = _.value.fold(_ => Option.empty, _.some)
        )
      )
    )

  val GmosNLongSlitType: ObjectType[Any, Modes.GmosNorthLongSlit] =
    ObjectType[Any, Modes.GmosNorthLongSlit](
      name        = "GmosNorthLongSlit",
      description = "Basic configuration for GMOS North Long Slit",

      fields[Any, Modes.GmosNorthLongSlit](
        Field(
          name        = "filter",
          fieldType   = OptionType(EnumTypeGmosNorthFilter),
          description = Some("GMOS North Filter"),
          resolve     = _.value.filter
        ),

        Field(
          name        = "grating",
          fieldType   = EnumTypeGmosNorthGrating,
          description = Some("GMOS North Grating"),
          resolve     = _.value.grating
        ),

        Field(
          name        = "fpu",
          fieldType   = EnumTypeGmosNorthFpu,
          description = Some("GMOS North FPU"),
          resolve     = _.value.fpu
        )
      )
    )

  val GmosSLongSlitType: ObjectType[Any, Modes.GmosSouthLongSlit] =
    ObjectType[Any, Modes.GmosSouthLongSlit](
      name        = "GmosSouthLongSlit",
      description = "Basic configuration for GMOS South Long Slit",

      fields[Any, Modes.GmosSouthLongSlit](
        Field(
          name        = "filter",
          fieldType   = OptionType(EnumTypeGmosSouthFilter),
          description = Some("GMOS South Filter"),
          resolve     = _.value.filter
        ),

        Field(
          name        = "grating",
          fieldType   = EnumTypeGmosSouthGrating,
          description = Some("GMOS South Grating"),
          resolve     = _.value.grating
        ),

        Field(
          name        = "fpu",
          fieldType   = EnumTypeGmosSouthFpu,
          description = Some("GMOS South  FPU"),
          resolve     = _.value.fpu
        )
      )
    )

}

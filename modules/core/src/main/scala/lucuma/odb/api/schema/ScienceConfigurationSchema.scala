// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.syntax.option._

import lucuma.core.math.Angle
import lucuma.odb.api.model.ConfigurationMode
import lucuma.odb.api.model.ScienceConfigurationModel
import lucuma.odb.api.model.ScienceConfigurationModel.Modes
import lucuma.odb.api.repo.OdbRepo
import lucuma.odb.api.schema.syntax.all._

import sangria.schema._
import sangria.macros.derive._

object ScienceConfigurationSchema {
  import GmosSchema._
  import InstrumentConfigSchema._

  implicit val EnumTypeConfigurationMode: EnumType[ConfigurationMode] =
    EnumType.fromEnumerated(
      "ConfigurationModeType",
      "ConfigurationMode"
    )

  implicit val EnumSlitWidthAngleUnits: EnumType[ScienceConfigurationModel.Units] =
    EnumType.fromEnumerated[ScienceConfigurationModel.Units](
      "SlitWidthAngleUnits",
      "Slit width units"
    )

  implicit val InputSlitWidthInput: InputType[ScienceConfigurationModel.SlitWidthInput] =
    deriveInputObjectType[ScienceConfigurationModel.SlitWidthInput](
      InputObjectTypeName("SlitWidthInput"),
      InputObjectTypeDescription("Slit width in appropriate units"),
    )

  def SlitWidthType[F[_]]: ObjectType[OdbRepo[F], Angle]=
    ObjectType(
      name     = "slitWidth",
      fieldsFn = () => fields(

        Field(
          name        = "microarcseconds",
          fieldType   = LongType,
          description = Some("Slit width in µas"),
          resolve     = v => Angle.signedMicroarcseconds.get(v.value)
        ),

        Field(
          name        = "milliarcseconds",
          fieldType   = BigDecimalType,
          description = Some("Slit width in mas"),
          resolve     = v => Angle.signedDecimalMilliarcseconds.get(v.value)
        ),

        Field(
          name        = "arcseconds",
          fieldType   = BigDecimalType,
          description = Some("Slit width in arcsec"),
          resolve     = v => Angle.signedDecimalArcseconds.get(v.value)
        )

      )
    )
  def ScienceConfigurationType[F[_]]: ObjectType[OdbRepo[F], ScienceConfigurationModel] =
    ObjectType[OdbRepo[F], ScienceConfigurationModel](
      name         = "ScienceConfiguration",
      description  = "Base science configuration",
      fields[OdbRepo[F], ScienceConfigurationModel](

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
          fieldType   = OptionType(GmosNLongSlitType[F]),
          description = "GMOS North Long Slit configuration".some,
          resolve     = _.value.fold(_.some, _ => Option.empty)
        ),

        Field(
          name        = "gmosSouthLongSlit",
          fieldType   = OptionType(GmosSLongSlitType[F]),
          description = "GMOS South Long Slit configuration".some,
          resolve     = _.value.fold(_ => Option.empty, _.some)
        )
      )
    )

  def GmosNLongSlitType[F[_]]: ObjectType[OdbRepo[F], Modes.GmosNorthLongSlit] =
    ObjectType[OdbRepo[F], Modes.GmosNorthLongSlit](
      name        = "GmosNorthLongSlit",
      description = "Basic configuration for GMOS North Long Slit",

      fields[OdbRepo[F], Modes.GmosNorthLongSlit](
        Field(
          name        = "filter",
          fieldType   = OptionType(EnumTypeGmosNorthFilter),
          description = Some("GMOS North Filter"),
          resolve     = _.value.filter
        ),

        Field(
          name        = "disperser",
          fieldType   = EnumTypeGmosNorthDisperser,
          description = Some("GMOS North Disperser"),
          resolve     = _.value.disperser
        ),

        Field(
          name        = "fpu",
          fieldType   = EnumTypeGmosNorthFpu,
          description = Some("GMOS North FPU"),
          resolve     = _.value.fpu
        ),

        Field(
          name        = "slitWidth",
          fieldType   = SlitWidthType[F],
          description = Some("Slit width in appropriate units"),
          resolve     = _.value.slitWidth
        ),
      )
    )

  def GmosSLongSlitType[F[_]]: ObjectType[OdbRepo[F], Modes.GmosSouthLongSlit] =
    ObjectType[OdbRepo[F], Modes.GmosSouthLongSlit](
      name        = "GmosSouthLongSlit",
      description = "Basic configuration for GMOS South Long Slit",

      fields[OdbRepo[F], Modes.GmosSouthLongSlit](
        Field(
          name        = "filter",
          fieldType   = OptionType(EnumTypeGmosSouthFilter),
          description = Some("GMOS South Filter"),
          resolve     = _.value.filter
        ),

        Field(
          name        = "disperser",
          fieldType   = EnumTypeGmosSouthDisperser,
          description = Some("GMOS South Disperser"),
          resolve     = _.value.disperser
        ),

        Field(
          name        = "fpu",
          fieldType   = EnumTypeGmosSouthFpu,
          description = Some("GMOS South  FPU"),
          resolve     = _.value.fpu
        ),

        Field(
          name        = "slitWidth",
          fieldType   = SlitWidthType[F],
          description = Some("Slit width in appropriate units"),
          resolve     = _.value.slitWidth
        ),
      )
    )

}

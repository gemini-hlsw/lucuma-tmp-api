// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{ScienceConfigurationInput, ScienceConfigurationModel}
import sangria.macros.derive._
import sangria.schema._

trait ScienceConfigurationMutation {

  import syntax.inputtype._
  import ScienceConfigurationModel.Modes._
  import ScienceConfigurationModel.SlitWidthInput
  import GmosSchema._

  implicit val InputSlitWidthInput: InputType[SlitWidthInput] =
    deriveInputObjectType[SlitWidthInput](
      InputObjectTypeName("SlitWidthInput"),
      InputObjectTypeDescription("Slit width in appropriate units"),
    )

  implicit val InputObjectTypeGmosSouthLongSlit: InputObjectType[GmosSouthLongSlitInput] =
    InputObjectType[GmosSouthLongSlitInput](
      "EditGmosSouthLongSlit",
      "Edit or create GMOS South Long Slit configuration",
      List(
        EnumTypeGmosSouthFilter.notNullableField("filter"),
        EnumTypeGmosSouthDisperser.notNullableField("disperser"),
        InputSlitWidthInput.notNullableField("slitWidth")
      )
    )

  implicit val InputObjectTypeGmosNorthLongSlit: InputObjectType[GmosNorthLongSlitInput] =
    InputObjectType[GmosNorthLongSlitInput](
      "EditGmosNorthLongSlit",
      "Edit or create GMOS North Long Slit configuration",
      List(
        EnumTypeGmosNorthFilter.notNullableField("filter"),
        EnumTypeGmosNorthDisperser.notNullableField("disperser"),
        InputSlitWidthInput.notNullableField("slitWidth")
      )
    )

  implicit val InputObjectTypeScienceConfig: InputObjectType[ScienceConfigurationInput] =
    InputObjectType[ScienceConfigurationInput](
      "ScienceConfigurationInput",
      "Edit or create an observation's science configuration",
      List(
        InputObjectTypeGmosNorthLongSlit.notNullableField("gmosNorthLongSlit"),
        InputObjectTypeGmosSouthLongSlit.notNullableField("gmosSouthLongSlit")
      )
    )

}

object ScienceConfigurationMutation extends ScienceConfigurationMutation

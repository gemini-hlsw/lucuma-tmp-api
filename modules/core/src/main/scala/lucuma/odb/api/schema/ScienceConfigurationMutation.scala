// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{ScienceConfigurationInput, ScienceConfigurationModel}
import sangria.schema._

trait ScienceConfigurationMutation {

  import syntax.inputtype._
  import ScienceConfigurationModel.Modes._
  import GmosSchema._

  implicit val InputObjectTypeGmosSouthLongSlit: InputObjectType[GmosSouthLongSlitInput] =
    InputObjectType[GmosSouthLongSlitInput](
      "GmosSouthLongSlitInput",
      "Edit or create GMOS South Long Slit configuration",
      List(
        EnumTypeGmosSouthFilter.notNullableField("filter"),
        EnumTypeGmosSouthGrating.notNullableField("grating"),
        EnumTypeGmosSouthFpu.notNullableField("fpu")
      )
    )

  implicit val InputObjectTypeGmosNorthLongSlit: InputObjectType[GmosNorthLongSlitInput] =
    InputObjectType[GmosNorthLongSlitInput](
      "GmosNorthLongSlitInput",
      "Edit or create GMOS North Long Slit configuration",
      List(
        EnumTypeGmosNorthFilter.notNullableField("filter"),
        EnumTypeGmosNorthGrating.notNullableField("grating"),
        EnumTypeGmosNorthFpu.notNullableField("fpu")
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

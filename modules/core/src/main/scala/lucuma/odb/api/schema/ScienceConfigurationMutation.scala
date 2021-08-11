// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.ScienceConfigurationModel
import lucuma.odb.api.model.ScienceConfigurationModel.ScienceConfigurationModelEdit
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

  implicit val InputObjectTypeCreateGmosNorthLongSlit: InputObjectType[CreateGmosNorthLongSlit] =
    deriveInputObjectType[CreateGmosNorthLongSlit](
      InputObjectTypeName("CreateGmosNorthLongSlit"),
      InputObjectTypeDescription("Create a configuration for a GMOS North Long Slit observation")
    )

  implicit val InputObjectTypeCreateGmosSouthLongSlit: InputObjectType[CreateGmosSouthLongSlit] =
    deriveInputObjectType[CreateGmosSouthLongSlit](
      InputObjectTypeName("CreateGmosSouthLongSlit"),
      InputObjectTypeDescription("Create a configuration for a GMOS South Long Slit observation")
    )

  implicit val InputObjectTypeScienceConfigurationCreate: InputObjectType[ScienceConfigurationModel.Create] =
    deriveInputObjectType[ScienceConfigurationModel.Create](
      InputObjectTypeName("CreateObservationConfigInput"),
      InputObjectTypeDescription("Create observation configuration"),
    )

  implicit val InputObjectTypeEditGmosSouthLongSlitEdit: InputObjectType[EditGmosSouthLongSlit] =
    deriveInputObjectType[EditGmosSouthLongSlit](
      InputObjectTypeName("EditGmosSouthLongSlit"),
      InputObjectTypeDescription("Edit GMOS South Long Slit configuration"),
      ReplaceInputField("filter", EnumTypeGmosSouthFilter.notNullableField("filter")),
      ReplaceInputField("disperser", EnumTypeGmosSouthDisperser.notNullableField("disperser")),
      ReplaceInputField("slitWidth", InputSlitWidthInput.notNullableField("slitWidth")),
    )

  implicit val InputObjectTypeEditGmosNorthLongSlitEdit: InputObjectType[EditGmosNorthLongSlit] =
    deriveInputObjectType[EditGmosNorthLongSlit](
      InputObjectTypeName("EditGmosNorthLongSlit"),
      InputObjectTypeDescription("Edit GMOS North Long Slit configuration"),
      ReplaceInputField("filter", EnumTypeGmosNorthFilter.notNullableField("filter")),
      ReplaceInputField("disperser", EnumTypeGmosNorthDisperser.notNullableField("disperser")),
      ReplaceInputField("slitWidth", InputSlitWidthInput.notNullableField("slitWidth")),
    )

  implicit val InputObjectTypeScienceConfigEdit: InputObjectType[ScienceConfigurationModel.Edit] =
    deriveInputObjectType[ScienceConfigurationModel.Edit](
      InputObjectTypeName("EditScienceConfiguration"),
      InputObjectTypeDescription("Edit observation configuration"),
      ReplaceInputField("gmosNorthLongSlit", InputObjectTypeEditGmosNorthLongSlitEdit.notNullableField("gmosNorthLongSlit")),
      ReplaceInputField("gmosSouthLongSlit", InputObjectTypeEditGmosSouthLongSlitEdit.notNullableField("gmosSouthLongSlit")),
    )

  implicit val InputObjectTypeScienceConfigurationSetEdit: InputObjectType[ScienceConfigurationModelEdit] =
    deriveInputObjectType[ScienceConfigurationModelEdit](
      InputObjectTypeName("EditScienceConfigurationInput"),
      InputObjectTypeDescription("Edit or set observation configuration"),
      ReplaceInputField("set", InputObjectTypeScienceConfigurationCreate.notNullableField("set")),
      ReplaceInputField("edit", InputObjectTypeScienceConfigEdit.notNullableField("edit")),
    )

}

object ScienceConfigurationMutation extends ScienceConfigurationMutation

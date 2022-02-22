// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.StepConfig.{CreateBias, CreateDark, CreateGcal, CreateScience, CreateStepConfig}
import sangria.schema._

object StepMutation {

  import GcalSchema._
  import OffsetSchema.InputObjectTypeOffsetInput

  def InputObjectTypeCreateBias[DI](
    typePrefix:  String,
    dynamicType: InputType[DI]
  ): InputObjectType[CreateBias[DI]] =

    InputObjectType[CreateBias[DI]](
      s"${typePrefix.capitalize}BiasInput",
      s"${typePrefix.capitalize} bias step creation input",
      List(
        InputField[DI]("config", dynamicType, "instrument configuration")
      )
    )

  def InputObjectTypeCreateDark[DI](
    typePrefix:  String,
    dynamicType: InputType[DI]
  ): InputObjectType[CreateDark[DI]] =

    InputObjectType[CreateDark[DI]](
      s"${typePrefix.capitalize}DarkInput",
      s"${typePrefix.capitalize} dark step creation input",
      List(
        InputField[DI]("config", dynamicType, "instrument configuration")
      )
    )

  def InputObjectTypeCreateGcal[DI](
    typePrefix:  String,
    dynamicType: InputType[DI]
  ): InputObjectType[CreateGcal[DI]] =

    InputObjectType[CreateGcal[DI]](
      s"${typePrefix.capitalize}GcalInput",
      s"${typePrefix.capitalize} GCAL step creation input",
      List(
        InputField("config",     dynamicType,                    "instrument configuration"),
        InputField("gcalConfig", InputObjectTypeGcalModelCreate, "GCAL configuration")
      )
    )

  def InputObjectTypeCreateScience[DI](
    typePrefix:  String,
    dynamicType: InputType[DI]
  ): InputObjectType[CreateScience[DI]] =

    InputObjectType[CreateScience[DI]](
      s"${typePrefix.capitalize}ScienceInput",
      s"${typePrefix.capitalize} science step creation input",
      List(
        InputField("config", dynamicType,                "instrument configuration"),
        InputField("offset", InputObjectTypeOffsetInput, "offset position")
      )
    )

  def InputObjectTypeCreateStepConfig[DI](
    typePrefix:  String,
    dynamicType: InputType[DI]
  ): InputObjectType[CreateStepConfig[DI]] =

    InputObjectType[CreateStepConfig[DI]](
      s"${typePrefix.capitalize}StepInput",
      s"${typePrefix.capitalize} step creation input.  Choose exactly one step type.",
      List(
        InputField("bias",    OptionInputType(InputObjectTypeCreateBias[DI](typePrefix, dynamicType)),    "Bias step creation option"),
        InputField("dark",    OptionInputType(InputObjectTypeCreateDark[DI](typePrefix, dynamicType)),    "Dark step creation option"),
        InputField("gcal",    OptionInputType(InputObjectTypeCreateGcal[DI](typePrefix, dynamicType)),    "GCAL step creation option"),
        InputField("science", OptionInputType(InputObjectTypeCreateScience[DI](typePrefix, dynamicType)), "Science step creation option")
      )
    )
}

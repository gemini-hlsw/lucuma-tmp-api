// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.syntax.all._
import lucuma.odb.api.model.{ExecutionModel, GmosModel, PlannedTime}
import lucuma.odb.api.repo.OdbCtx
import sangria.schema.{Field, _}


object ManualConfigSchema {

  import GmosSchema.{GmosNorthDynamicType, GmosNorthStaticConfigType, GmosSouthDynamicType, GmosSouthStaticConfigType}
  import InstrumentSchema.EnumTypeInstrument
  import PlannedTimeSchema.PlannedTimeType
  import SequenceSchema.SequenceType

  def ManualConfigType[F[_]]: InterfaceType[OdbCtx[F], ExecutionModel] =
    InterfaceType[OdbCtx[F], ExecutionModel](
      name        = "ManualConfig",
      description = "Manual observing sequence and instrument configuration",
      fields[OdbCtx[F], ExecutionModel](

        Field(
          name        = "instrument",
          fieldType   = EnumTypeInstrument,
          description = "Instrument type".some,
          resolve     = _.value.instrument
        ),

        Field(
          name        = "plannedTime",
          fieldType   = PlannedTimeType,
          description = Some("Planned time for this configuration"),
          resolve     = c => PlannedTime.estimate(c.value)
        )

      )
    )

  def manualConfigFields[F[_], S, D, C <: ExecutionModel](
    typePrefix:  String,
    staticType:  OutputType[S],
    dynamicType: OutputType[D],
    f:           C => ExecutionModel.Config[S, D]
  ): List[Field[OdbCtx[F], C]] =

    List(

      Field(
        name        = "static",
        fieldType   = staticType,
        description = Some("Static/unchanging configuration"),
        resolve     = c => f(c.value).static
      ),

      Field(
        name        = "acquisition",
        fieldType   = SequenceType[F, D](typePrefix, dynamicType),
        description = Some("Acquisition sequence."),
        resolve     = c => f(c.value).acquisition
      ),

      Field(
        name        = "science",
        fieldType   = SequenceType[F, D](typePrefix, dynamicType),
        description = Some("Science sequence."),
        resolve     = c => f(c.value).science
      )
    )

  def GmosNorthManualConfigType[F[_]]: ObjectType[OdbCtx[F], ExecutionModel.GmosNorth] =
    ObjectType(
      name        = "GmosNorthManualConfig",
      description = "GMOS North Manual Configuration",
      interfaces  = List(PossibleInterface.apply[OdbCtx[F], ExecutionModel.GmosNorth](ManualConfigType[F])),
      fields      = manualConfigFields[F, GmosModel.NorthStatic, GmosModel.NorthDynamic, ExecutionModel.GmosNorth](
        "GmosNorth",
        GmosNorthStaticConfigType,
        GmosNorthDynamicType,
        _.config
      )
    )

  def GmosSouthManualConfigType[F[_]]: ObjectType[OdbCtx[F], ExecutionModel.GmosSouth] =
    ObjectType(
      name        = "GmosSouthManualConfig",
      description = "GMOS South Manual Configuration",
      interfaces  = List(PossibleInterface.apply[OdbCtx[F], ExecutionModel.GmosSouth](ManualConfigType[F])),
      fields      = manualConfigFields[F, GmosModel.SouthStatic, GmosModel.SouthDynamic, ExecutionModel.GmosSouth](
        "GmosSouth",
        GmosSouthStaticConfigType,
        GmosSouthDynamicType,
        _.config
      )
    )

  def manualConfigImplementations[F[_]]: List[Type with Named] =
    List(
      GmosNorthManualConfigType[F],
      GmosSouthManualConfigType[F]
    )

}

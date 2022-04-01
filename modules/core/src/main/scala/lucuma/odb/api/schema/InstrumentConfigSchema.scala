// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.`enum`.Instrument
import lucuma.odb.api.model.{InstrumentConfigModel, PlannedTime, Sequence}
import lucuma.odb.api.repo.OdbCtx
import lucuma.odb.api.schema.TimeSchema.DurationType
import sangria.schema._


object InstrumentConfigSchema {

  import GmosSchema._
  import PlannedTimeSchema._
  import SequenceSchema._
  import syntax.`enum`._

  implicit val EnumTypeInstrument: EnumType[Instrument] =
    EnumType.fromEnumerated(
      "InstrumentType",
      "Instrument"
    )

  def ConfigType[F[_]]: InterfaceType[OdbCtx[F], InstrumentConfigModel] =
    InterfaceType[OdbCtx[F], InstrumentConfigModel](
      name        = "Config",
      description = "Instrument configuration",
      fields[OdbCtx[F], InstrumentConfigModel](

        Field(
          name        = "instrument",
          fieldType   = EnumTypeInstrument,
          description = Some("Instrument type"),
          resolve     = _.value.instrument
        ),

        Field(
          name        = "plannedTime",
          fieldType   = PlannedTimeType,
          description = Some("Planned time for this configuration"),
          resolve     = c => PlannedTime.estimate(c.value)
        ),

        Field(
          name        = "setupTime",
          fieldType   = DurationType,
          description = Some("Estimated setup time"),
          resolve     = c => PlannedTime.estimate(c.value).setup.value
        )

      )
    )

  def implementations[F[_]]: List[Type with Named] =
    List(
      GmosNorthConfigType[F],
      GmosSouthConfigType[F]
    )

  def instrumentConfigFields[F[_], I <: InstrumentConfigModel, S, D](
    typePrefix:  String,
    staticType:  OutputType[S],
    dynamicType: OutputType[D],
    static:       I => S,
    acquisition:  I => Sequence[D],
    science:      I => Sequence[D]
  ): List[Field[OdbCtx[F], I]] =

    List(

      Field(
        name        = "static",
        fieldType   = staticType,
        description = Some("Static/unchanging configuration"),
        resolve     = c => static(c.value)
      ),

      Field(
        name        = "acquisition",
        fieldType   = SequenceType[F, D](typePrefix, dynamicType),
        description = Some("Acquisition sequence."),
        resolve     = c => acquisition(c.value)
      ),

      Field(
        name        = "science",
        fieldType   = SequenceType[F, D](typePrefix, dynamicType),
        description = Some("Science sequence."),
        resolve     = c => science(c.value)
      )
    )

  def GmosNorthConfigType[F[_]]: ObjectType[OdbCtx[F], InstrumentConfigModel.GmosNorth] =
    ObjectType(
      name        = "GmosNorthConfig",
      description = "GMOS North Configuration",
      interfaces  = List(PossibleInterface.apply[OdbCtx[F], InstrumentConfigModel.GmosNorth](ConfigType[F])),
      fields      = instrumentConfigFields(
        "GmosNorth",
        GmosNorthStaticConfigType,
        GmosNorthDynamicType,
        _.static,
        _.acquisition,
        _.science
      )
    )

  def GmosSouthConfigType[F[_]]: ObjectType[OdbCtx[F], InstrumentConfigModel.GmosSouth] =
    ObjectType(
      name        = "GmosSouthConfig",
      description = "GMOS South Configuration",
      interfaces  = List(PossibleInterface.apply[OdbCtx[F], InstrumentConfigModel.GmosSouth](ConfigType)),
      fields      = instrumentConfigFields(
        "GmosSouth",
        GmosSouthStaticConfigType,
        GmosSouthDynamicType,
        _.static,
        _.acquisition,
        _.science
      )
    )

}

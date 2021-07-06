// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.`enum`.Instrument
import lucuma.odb.api.model.{DereferencedSequence, InstrumentConfigModel, PlannedTime}
import lucuma.odb.api.repo.OdbRepo
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

  def ConfigType[F[_]]: InterfaceType[OdbRepo[F], InstrumentConfigModel] =
    InterfaceType[OdbRepo[F], InstrumentConfigModel](
      name        = "Config",
      description = "Instrument configuration",
      fields[OdbRepo[F], InstrumentConfigModel](

        Field(
          name        = "instrument",
          fieldType   = EnumTypeInstrument,
          description = Some("Instrument type"),
          resolve     = _.value.instrument
        ),

        Field(
          name        = "plannedTime",
          fieldType   = PlannedTimeType[F],
          description = Some("Planned time for this configuration"),
          resolve     = c => PlannedTime.estimate(c.value)
        ),

        Field(
          name        = "setupTime",
          fieldType   = DurationType[F],
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
    acquisition:  I => DereferencedSequence[D],
    science:      I => DereferencedSequence[D]
  ): List[Field[OdbRepo[F], I]] =

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

  def GmosNorthConfigType[F[_]]: ObjectType[OdbRepo[F], InstrumentConfigModel.GmosNorth] =
    ObjectType(
      name        = "GmosNorthConfig",
      description = "GMOS North Configuration",
      interfaces  = List(PossibleInterface.apply[OdbRepo[F], InstrumentConfigModel.GmosNorth](ConfigType[F])),
      fields      = instrumentConfigFields(
        "GmosNorth",
        GmosNorthStaticConfigType[F],
        GmosNorthDynamicType[F],
        _.static,
        _.acquisition,
        _.science
      )
    )

  def GmosSouthConfigType[F[_]]: ObjectType[OdbRepo[F], InstrumentConfigModel.GmosSouth] =
    ObjectType(
      name        = "GmosSouthConfig",
      description = "GMOS South Configuration",
      interfaces  = List(PossibleInterface.apply[OdbRepo[F], InstrumentConfigModel.GmosSouth](ConfigType[F])),
      fields      = instrumentConfigFields(
        "GmosSouth",
        GmosSouthStaticConfigType[F],
        GmosSouthDynamicType[F],
        _.static,
        _.acquisition,
        _.science
      )
    )

}

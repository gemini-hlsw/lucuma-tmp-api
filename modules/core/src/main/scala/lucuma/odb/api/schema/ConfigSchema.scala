// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.`enum`.Instrument
import lucuma.odb.api.model.SequenceModel._
import lucuma.odb.api.repo.OdbRepo
import cats.effect.Effect
import lucuma.odb.api.model.PlannedTime
import sangria.schema._


object ConfigSchema {

  import GmosSchema._
  import PlannedTimeSchema._
  import SequenceSchema._
  import syntax.`enum`._

  implicit val EnumTypeInstrument: EnumType[Instrument] =
    EnumType.fromEnumerated(
      "InstrumentType",
      "Instrument"
    )

  def ConfigType[F[_]: Effect]: InterfaceType[OdbRepo[F], InstrumentConfig] =
    InterfaceType[OdbRepo[F], InstrumentConfig](
      name        = "Config",
      description = "Instrument configuration",
      fields[OdbRepo[F], InstrumentConfig](

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
        )

      )
    )

  def implementations[F[_]: Effect]: List[Type with Named] =
    List(
      GmosNorthConfigType[F],
      GmosSouthConfigType[F]
    )

  def GmosNorthConfigType[F[_]: Effect]: ObjectType[OdbRepo[F], InstrumentConfig.GmosNorth] =
    ObjectType(
      name        = "GmosNorthConfig",
      description = "GMOS North Configuration",
      interfaces  = List(PossibleInterface.apply[OdbRepo[F], InstrumentConfig.GmosNorth](ConfigType[F])),
      fields      = instrumentConfigFields(
        "GmosNorth",
        GmosNorthStaticConfigType[F],
        GmosNorthDynamicType[F],
        _.config
      )
    )

  def GmosSouthConfigType[F[_]: Effect]: ObjectType[OdbRepo[F], InstrumentConfig.GmosSouth] =
    ObjectType(
      name        = "GmosSouthConfig",
      description = "GMOS South Configuration",
      interfaces  = List(PossibleInterface.apply[OdbRepo[F], InstrumentConfig.GmosSouth](ConfigType[F])),
      fields      = instrumentConfigFields(
        "GmosSouth",
        GmosSouthStaticConfigType[F],
        GmosSouthDynamicType[F],
        _.config
      )
    )

}

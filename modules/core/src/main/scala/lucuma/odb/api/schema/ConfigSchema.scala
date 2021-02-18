// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.`enum`.Instrument
import lucuma.odb.api.model.{ConfigModel, GmosModel}
import lucuma.odb.api.model.SequenceModel._
import lucuma.odb.api.repo.OdbRepo
import cats.effect.Effect
import sangria.schema._


object ConfigSchema {

  import GmosSchema._
  import InstrumentSequenceSchema._
  import syntax.`enum`._

  implicit val EnumTypeInstrument: EnumType[Instrument] =
    EnumType.fromEnumerated(
      "InstrumentType",
      "Instrument"
    )

  def ConfigType[F[_]: Effect]: InterfaceType[OdbRepo[F], ConfigModel] =
    InterfaceType[OdbRepo[F], ConfigModel](
      name        = "Config",
      description = "Instrument configuration",
      fields[OdbRepo[F], ConfigModel](

        Field(
          name        = "instrument",
          fieldType   = EnumTypeInstrument,
          description = Some("Instrument type"),
          resolve     = _.value.instrument
        )

      )
    )

  def implementations[F[_]: Effect]: List[Type with Named] =
    List(
      GmosNorthConfigType[F],
      GmosSouthConfigType[F]
    )

  def GmosNorthSequenceType[F[_]: Effect]: ObjectType[OdbRepo[F], Sequence[GmosModel.NorthStatic, GmosModel.NorthDynamic]] =
    InstrumentSequenceType(
      "GmosNorth",
      "Instrument sequence",
      GmosNorthStaticConfigType[F],
      GmosNorthDynamicType[F]
    )

  def GmosNorthConfigType[F[_]: Effect]: ObjectType[OdbRepo[F], ConfigModel.GmosNorth] =
    ObjectType(
      name        = "GmosNorthConfig",
      description = "GMOS North Configuration",
      interfaces  = List(PossibleInterface.apply[OdbRepo[F], ConfigModel.GmosNorth](ConfigType[F])),
      fields      = List(

        Field(
          name        = "manual",
          fieldType   = GmosNorthSequenceType[F],
          description = Some("GMOS North manual sequence configuration"),
          resolve     = _.value.manual
        )

      )
    )

  def GmosSouthSequenceType[F[_]: Effect]: ObjectType[OdbRepo[F], Sequence[GmosModel.SouthStatic, GmosModel.SouthDynamic]] =
    InstrumentSequenceType(
      "GmosSouth",
      "Instrument sequence",
      GmosSouthStaticConfigType[F],
      GmosSouthDynamicType[F]
    )


  def GmosSouthConfigType[F[_]: Effect]: ObjectType[OdbRepo[F], ConfigModel.GmosSouth] =
    ObjectType(
      name        = "GmosSouthConfig",
      description = "GMOS South Configuration",
      interfaces  = List(PossibleInterface.apply[OdbRepo[F], ConfigModel.GmosSouth](ConfigType[F])),
      fields      = List(

        Field(
          name        = "manual",
          fieldType   = GmosSouthSequenceType[F],
          description = Some("GMOS South manual sequence configuration"),
          resolve     = _.value.manual
        )

      )
    )
}

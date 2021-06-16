// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.model.Observation
import lucuma.odb.api.model.{DereferencedSequence, GmosModel}
import lucuma.odb.api.repo.OdbRepo
import cats.effect.Effect
import cats.syntax.all._
import lucuma.core.`enum`.Instrument
import sangria.schema.{Field, _}

object ExecutionConfigSchema {

  import context._
  import AtomSchema.AtomConcreteType
  import InstrumentConfigSchema.EnumTypeInstrument

  final case class ExecutionContext[S, D](
    oid:         Observation.Id,
    instrument:  Instrument,
    static:      S,
    acquisition: DereferencedSequence[D],
    science:     DereferencedSequence[D]
  )

  def ExecutionConfigType[F[_]: Effect]: InterfaceType[OdbRepo[F], ExecutionContext[_, _]] =
    InterfaceType[OdbRepo[F], ExecutionContext[_, _]](
      name        = "ExecutionConfig",
      description = "Execution configuration",
      fields[OdbRepo[F], ExecutionContext[_, _]](

        Field(
          name        = "instrument",
          fieldType   = EnumTypeInstrument,
          description = "Instrument type".some,
          resolve     = _.value.instrument
        )

      )
    )

  def implementations[F[_]: Effect]: List[Type with Named] =
    List(
      GmosNorthExecutionConfigType[F],
      GmosSouthExecutionConfigType[F]
    )

  def executionSequence[F[_]: Effect, D](
    instrument:  Instrument,
    dynamicType: OutputType[D],
    sequence:    ExecutionContext[_, D] => DereferencedSequence[D]
  ): ObjectType[OdbRepo[F], ExecutionContext[_, D]] =
    ObjectType(
      name        = s"${instrument.tag}ExecutionSequence",
      description = s"Next atom to execute and potential future atoms",
      fieldsFn    = () => fields(

        Field(
          name        = "nextAtom",
          fieldType   = OptionType(AtomConcreteType[F, D](instrument.tag, dynamicType)),
          description = "Next atom to execute, if any".some,
          resolve     = c =>
            c.executionEvent(
              _.selectRemainingAtoms[D](c.value.oid, sequence(c.value))
               .map(_.headOption)
            )
        ),

        Field(
          name        = "possibleFuture",
          fieldType   = ListType(AtomConcreteType[F, D](instrument.tag, dynamicType)),
          description = "Remaining atoms to execute, if any".some,
          resolve     = c =>
            c.executionEvent(
              _.selectRemainingAtoms[D](c.value.oid, sequence(c.value))
               .map(_.drop(1))
            )
        )
      )
    )

  def ConcreteExecutionConfigType[F[_]: Effect, S, D](
    instrument:  Instrument,
    staticType:  OutputType[S],
    dynamicType: OutputType[D]
  ): ObjectType[OdbRepo[F], ExecutionContext[S, D]] =
    ObjectType(
      name        = s"${instrument.tag}ExecutionConfig",
      description = s"${instrument.longName} Execution Config",
      interfaces  = List(PossibleInterface.apply[OdbRepo[F], ExecutionContext[S, D]](ExecutionConfigType[F])),
      fieldsFn    = () => fields(

        Field(
          name        = "static",
          fieldType   = staticType,
          description = s"${instrument.longName} static configuration".some,
          resolve     = _.value.static
        ),

        Field(
          name        = "acquisition",
          fieldType   = executionSequence[F, D](instrument, dynamicType, _.acquisition),
          description = s"${instrument.longName} acquisition execution".some,
          resolve     = _.value
        ),

        Field(
          name        = "science",
          fieldType   = executionSequence[F, D](instrument, dynamicType, _.science),
          description = s"${instrument.longName} science execution".some,
          resolve     = _.value
        )

      )
    )


  def GmosNorthExecutionConfigType[F[_]: Effect]: ObjectType[OdbRepo[F], ExecutionContext[GmosModel.NorthStatic, GmosModel.NorthDynamic]] =
    ConcreteExecutionConfigType[F, GmosModel.NorthStatic, GmosModel.NorthDynamic](
      Instrument.GmosNorth,
      GmosSchema.GmosNorthStaticConfigType[F],
      GmosSchema.GmosNorthDynamicType[F]
    )

  def GmosSouthExecutionConfigType[F[_]: Effect]: ObjectType[OdbRepo[F], ExecutionContext[GmosModel.SouthStatic, GmosModel.SouthDynamic]] =
    ConcreteExecutionConfigType[F, GmosModel.SouthStatic, GmosModel.SouthDynamic](
      Instrument.GmosSouth,
      GmosSchema.GmosSouthStaticConfigType[F],
      GmosSchema.GmosSouthDynamicType[F]
    )
}

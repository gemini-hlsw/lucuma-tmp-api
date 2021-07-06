// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.Functor
import cats.effect.std.Dispatcher
import lucuma.core.model.Observation
import lucuma.odb.api.model.{DereferencedSequence, GmosModel, SequenceModel}
import lucuma.odb.api.model.SequenceModel.SequenceType.{Acquisition, Science}
import lucuma.odb.api.repo.OdbRepo
import cats.syntax.all._
import lucuma.core.`enum`.Instrument
import sangria.schema.{Field, _}

object ExecutionConfigSchema {

  import context._
  import AtomSchema.AtomConcreteType
  import InstrumentConfigSchema.EnumTypeInstrument

  sealed trait ExecutionContext[S, D] extends Product with Serializable {
    def oid: Observation.Id

    def instrument: Instrument

    def static: S

    def acquisition: DereferencedSequence[D]

    def science: DereferencedSequence[D]

    def sequence(sequenceType: SequenceModel.SequenceType): DereferencedSequence[D] =
      sequenceType match {
        case Acquisition => acquisition
        case Science     => science
      }
  }

  final case class GmosNorthExecutionContext(
    oid:         Observation.Id,
    instrument:  Instrument,
    static:      GmosModel.NorthStatic,
    acquisition: DereferencedSequence[GmosModel.NorthDynamic],
    science:     DereferencedSequence[GmosModel.NorthDynamic]
  ) extends ExecutionContext[GmosModel.NorthStatic, GmosModel.NorthDynamic]

  final case class GmosSouthExecutionContext(
    oid:         Observation.Id,
    instrument:  Instrument,
    static:      GmosModel.SouthStatic,
    acquisition: DereferencedSequence[GmosModel.SouthDynamic],
    science:     DereferencedSequence[GmosModel.SouthDynamic]
  ) extends ExecutionContext[GmosModel.SouthStatic, GmosModel.SouthDynamic]

  def ExecutionConfigType[F[_]]: InterfaceType[OdbRepo[F], ExecutionContext[_, _]] =
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

  def implementations[F[_]: Functor: Dispatcher]: List[Type with Named] =
    List(
      GmosNorthExecutionConfigType[F],
      GmosSouthExecutionConfigType[F]
    )

  def executionSequence[F[_]: Functor: Dispatcher, D](
    instrument:   Instrument,
    dynamicType:  OutputType[D],
    sequenceType: SequenceModel.SequenceType
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
              _.selectRemainingAtoms(c.value.oid, sequenceType)
               .map(
                 _.headOption.flatMap { atom =>
                   c.value.sequence(sequenceType).atoms.find(_.id === atom.id)
                 }
               )
            )
        ),

        Field(
          name        = "possibleFuture",
          fieldType   = ListType(AtomConcreteType[F, D](instrument.tag, dynamicType)),
          description = "Remaining atoms to execute, if any".some,
          resolve     = c => {
            val allDeref = c.value.sequence(sequenceType).atoms.fproductLeft(_.id).toMap

            c.executionEvent(
              _.selectRemainingAtoms(c.value.oid, sequenceType)
               .map(
                 _.drop(1).flatMap { atom => allDeref.get(atom.id).toList }
               )
            )

          }
        )
      )
    )

  def executionConfigFields[F[_]: Functor: Dispatcher, S, D, E <: ExecutionContext[S, D]](
    instrument:  Instrument,
    staticType:  OutputType[S],
    dynamicType: OutputType[D]
  ): List[Field[OdbRepo[F], E]] =

    List(
      Field(
        name        = "static",
        fieldType   = staticType,
        description = s"${instrument.longName} static configuration".some,
        resolve     = _.value.static
      ),

      Field(
        name        = "acquisition",
        fieldType   = executionSequence[F, D](instrument, dynamicType, Acquisition),
        description = s"${instrument.longName} acquisition execution".some,
        resolve     = _.value
      ),

      Field(
        name        = "science",
        fieldType   = executionSequence[F, D](instrument, dynamicType, Science),
        description = s"${instrument.longName} science execution".some,
        resolve     = _.value
      )
    )

  private def executionConfigName(instrument: Instrument): String =
    s"${instrument.tag}ExecutionConfig"

  private def executionConfigDescription(instrument: Instrument): String =
    s"${instrument.longName} Execution Config"

  def GmosNorthExecutionConfigType[F[_]: Functor: Dispatcher]: ObjectType[OdbRepo[F], GmosNorthExecutionContext] =
    ObjectType(
      name        = executionConfigName(Instrument.GmosNorth),
      description = executionConfigDescription(Instrument.GmosNorth),
      interfaces  = List(PossibleInterface.apply[OdbRepo[F], GmosNorthExecutionContext](ExecutionConfigType[F])),
      fields      = executionConfigFields(
        Instrument.GmosNorth,
        GmosSchema.GmosNorthStaticConfigType[F],
        GmosSchema.GmosNorthDynamicType[F]
      )
    )

  def GmosSouthExecutionConfigType[F[_]: Functor: Dispatcher]: ObjectType[OdbRepo[F], GmosSouthExecutionContext] =
    ObjectType(
      name        = executionConfigName(Instrument.GmosSouth),
      description = executionConfigDescription(Instrument.GmosSouth),
      interfaces  = List(PossibleInterface.apply[OdbRepo[F], GmosSouthExecutionContext](ExecutionConfigType[F])),
      fields      = executionConfigFields(
        Instrument.GmosSouth,
        GmosSchema.GmosSouthStaticConfigType[F],
        GmosSchema.GmosSouthDynamicType[F]
      )
    )
}

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.Functor
import cats.effect.Async
import cats.effect.std.Dispatcher
import lucuma.core.model.Observation
import lucuma.odb.api.model.{GmosModel, InstrumentConfigModel, Sequence, SequenceModel, Visit, VisitRecord, VisitRecords}
import lucuma.odb.api.model.SequenceModel.SequenceType.{Acquisition, Science}
import cats.syntax.all._
import lucuma.core.`enum`.Instrument
import lucuma.odb.api.repo.OdbCtx
import lucuma.odb.api.schema.Paging.unsafeSelectPageFuture
import org.typelevel.log4cats.Logger
import sangria.schema.{Field, _}

import scala.collection.immutable.ListMap

object ExecutionConfigSchema {

  import context._
  import AtomSchema.AtomConcreteType
  import InstrumentConfigSchema.EnumTypeInstrument
  import Paging.{ArgumentPagingCursor, ArgumentPagingFirst, Cursor}

  sealed trait ExecutionContext[S, D] extends Product with Serializable {
    def oid: Observation.Id

    def instrument: Instrument

    def static: S

    def acquisition: Sequence[D]

    def science: Sequence[D]

    def sequence(sequenceType: SequenceModel.SequenceType): Sequence[D] =
      sequenceType match {
        case Acquisition => acquisition
        case Science     => science
      }
  }

  final case class GmosNorthExecutionContext(
    oid:         Observation.Id,
    instrument:  Instrument,
    static:      GmosModel.NorthStatic,
    acquisition: Sequence[GmosModel.NorthDynamic],
    science:     Sequence[GmosModel.NorthDynamic]
  ) extends ExecutionContext[GmosModel.NorthStatic, GmosModel.NorthDynamic]

  final case class GmosSouthExecutionContext(
    oid:         Observation.Id,
    instrument:  Instrument,
    static:      GmosModel.SouthStatic,
    acquisition: Sequence[GmosModel.SouthDynamic],
    science:     Sequence[GmosModel.SouthDynamic]
  ) extends ExecutionContext[GmosModel.SouthStatic, GmosModel.SouthDynamic]

  def ExecutionConfigType[F[_]]: InterfaceType[OdbCtx[F], ExecutionContext[_, _]] =
    InterfaceType[OdbCtx[F], ExecutionContext[_, _]](
      name        = "ExecutionConfig",
      description = "Execution configuration",
      fields[OdbCtx[F], ExecutionContext[_, _]](

        Field(
          name        = "instrument",
          fieldType   = EnumTypeInstrument,
          description = "Instrument type".some,
          resolve     = _.value.instrument
        )

      )
    )

  def implementations[F[_]: Dispatcher: Async: Logger]: List[Type with Named] =
    List(
      GmosNorthExecutionConfigType[F],
      GmosSouthExecutionConfigType[F]
    )

  def executionSequence[F[_]: Functor: Dispatcher, S, D](
    instrument:  Instrument,
    dynamicType: OutputType[D],
    recs:        VisitRecords => Option[ListMap[Visit.Id, VisitRecord[S, D]]],
    seq:         InstrumentConfigModel => Option[Sequence[D]]
  ): ObjectType[OdbCtx[F], ExecutionContext[_, D]] =
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
              _.selectRemainingAtoms(c.value.oid, recs, seq)
               .map(_.atoms.headOption)
            )
        ),

        Field(
          name        = "possibleFuture",
          fieldType   = ListType(AtomConcreteType[F, D](instrument.tag, dynamicType)),
          description = "Remaining atoms to execute, if any".some,
          resolve     = c =>
            c.executionEvent(
              _.selectRemainingAtoms(c.value.oid, recs, seq).map(_.atoms.drop(1))
            )
        )
      )
    )

  def executionConfigFields[F[_]: Dispatcher: Async: Logger, S, D, E <: ExecutionContext[S, D]](
    instrument:  Instrument,
    staticType:  OutputType[S],
    dynamicType: OutputType[D],
    recs:        VisitRecords => Option[ListMap[Visit.Id, VisitRecord[S, D]]],
    acquisition: InstrumentConfigModel => Option[Sequence[D]],
    science:     InstrumentConfigModel => Option[Sequence[D]]
  ): List[Field[OdbCtx[F], E]] =

    List(
      Field(
        name        = "static",
        fieldType   = staticType,
        description = s"${instrument.longName} static configuration".some,
        resolve     = _.value.static
      ),

      Field(
        name        = "acquisition",
        fieldType   = executionSequence[F, S, D](instrument, dynamicType, recs, acquisition),
        description = s"${instrument.longName} acquisition execution".some,
        resolve     = _.value
      ),

      Field(
        name        = "science",
        fieldType   = executionSequence[F, S, D](instrument, dynamicType, recs, science),
        description = s"${instrument.longName} science execution".some,
        resolve     = _.value
      ),

      Field(
        name        = "visits",
        fieldType   = VisitRecordSchema.visitRecordConnectionType[F, S, D](instrument.tag, staticType, dynamicType),
        arguments   = List(
          ArgumentPagingFirst,
          ArgumentPagingCursor
        ),
        resolve     = c =>
          unsafeSelectPageFuture[F, Visit.Id, VisitRecord.Output[S, D]](
            c.pagingVisitId,
            (r: VisitRecord.Output[S, D]) => Cursor.uid[Visit.Id].reverseGet(r.visitId),
            vid => c.ctx.odbRepo.executionEvent.selectVisitsPageForObservation[S, D](
              c.value.oid,
              recs,
              c.pagingFirst,
              vid
            )
          )
      )

    )

  private def executionConfigName(instrument: Instrument): String =
    s"${instrument.tag}ExecutionConfig"

  private def executionConfigDescription(instrument: Instrument): String =
    s"${instrument.longName} Execution Config"

  def GmosNorthExecutionConfigType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], GmosNorthExecutionContext] =
    ObjectType(
      name        = executionConfigName(Instrument.GmosNorth),
      description = executionConfigDescription(Instrument.GmosNorth),
      interfaces  = List(PossibleInterface.apply[OdbCtx[F], GmosNorthExecutionContext](ExecutionConfigType[F])),
      fields      = executionConfigFields(
        Instrument.GmosNorth,
        GmosSchema.GmosNorthStaticConfigType,
        GmosSchema.GmosNorthDynamicType,
        VisitRecords.gmosNorthVisits.getOption,
        _.gmosNorth.map(_.acquisition),
        _.gmosNorth.map(_.science)
      )
    )

  def GmosSouthExecutionConfigType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], GmosSouthExecutionContext] =
    ObjectType(
      name        = executionConfigName(Instrument.GmosSouth),
      description = executionConfigDescription(Instrument.GmosSouth),
      interfaces  = List(PossibleInterface.apply[OdbCtx[F], GmosSouthExecutionContext](ExecutionConfigType[F])),
      fields      = executionConfigFields(
        Instrument.GmosSouth,
        GmosSchema.GmosSouthStaticConfigType,
        GmosSchema.GmosSouthDynamicType,
        VisitRecords.gmosSouthVisits.getOption,
        _.gmosSouth.map(_.acquisition),
        _.gmosSouth.map(_.science)
      )
    )
}

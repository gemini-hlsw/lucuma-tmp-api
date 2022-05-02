// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.all._
import eu.timepit.refined.types.all.PosInt
import lucuma.core.`enum`.Instrument
import lucuma.core.model.{ExecutionEvent, Observation}
import lucuma.gen.SequenceComputation
import lucuma.odb.api.model._
import lucuma.odb.api.repo.OdbCtx
import org.typelevel.log4cats.Logger
import sangria.schema._


object ExecutionSchema {

  import AtomSchema.AtomConcreteType
  import context._
  import DatasetSchema._
  import ExecutionEventSchema._
  import InstrumentSchema.EnumTypeInstrument
  import GmosSchema.{GmosNorthDynamicType, GmosNorthStaticConfigType, GmosSouthDynamicType, GmosSouthStaticConfigType}
  import Paging._


  def ExecutionConfigType[F[_]]: InterfaceType[OdbCtx[F], ExecutionContext] =
    InterfaceType[OdbCtx[F], ExecutionContext](
      name        = "ExecutionConfig",
      description = "Execution configuration",
      fields[OdbCtx[F], ExecutionContext](
        Field(
          name        = "instrument",
          fieldType   = EnumTypeInstrument,
          description = "Instrument type".some,
          resolve     = _.value.exec.instrument
        )
      )
    )

  def executionConfigImplementations[F[_]: Dispatcher: Async: Logger]: List[Type with Named] =
    List(
      GmosNorthExecutionConfigType[F],
      GmosSouthExecutionConfigType[F]
    )

  def executionSequence[F[_], S, D](
    instrument:  Instrument,
    dynamicType: OutputType[D]
  ): ObjectType[OdbCtx[F], Sequence[D]] =
    ObjectType(
      name        = s"${instrument.tag}ExecutionSequence",
      description = s"Next atom to execute and potential future atoms",
      fieldsFn    = () => fields(

        Field(
          name        = "nextAtom",
          fieldType   = OptionType(AtomConcreteType[F, D](instrument.tag, dynamicType)),
          description = "Next atom to execute, if any".some,
          resolve     = _.value.atoms.headOption
        ),

        Field(
          name        = "possibleFuture",
          fieldType   = ListType(AtomConcreteType[F, D](instrument.tag, dynamicType)),
          description = "Remaining atoms to execute, if any".some,
          resolve     = _.value.atoms.drop(1)
        )
      )
    )

  def executionConfigFields[F[_]: Dispatcher: Async: Logger, S, D, C <: ExecutionContext](
    instrument:  Instrument,
    staticType:  OutputType[S],
    dynamicType: OutputType[D],
    visits:      VisitRecords => List[(Visit.Id, VisitRecord[S, D])],
    config:      C => ExecutionModel.Config[S, D]
  ): List[Field[OdbCtx[F], C]] =

    List(
      Field(
        name        = "static",
        fieldType   = staticType,
        description = s"${instrument.longName} static configuration".some,
        resolve     = c => config(c.value).static
      ),

      Field(
        name        = "acquisition",
        fieldType   = executionSequence[F, S, D](instrument, dynamicType),
        description = s"${instrument.longName} acquisition execution".some,
        resolve     = c => config(c.value).acquisition
      ),

      Field(
        name        = "science",
        fieldType   = executionSequence[F, S, D](instrument, dynamicType),
        description = s"${instrument.longName} science execution".some,
        resolve     = c => config(c.value).science
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
              visits,
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

  def GmosNorthExecutionConfigType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], ExecutionContext.GmosNorth] =
    ObjectType(
      name        = executionConfigName(Instrument.GmosNorth),
      description = executionConfigDescription(Instrument.GmosNorth),
      interfaces  = List(PossibleInterface.apply[OdbCtx[F], ExecutionContext.GmosNorth](ExecutionConfigType[F])),
      fields      = executionConfigFields[F, GmosModel.NorthStatic, GmosModel.NorthDynamic, ExecutionContext.GmosNorth](
        Instrument.GmosNorth,
        GmosNorthStaticConfigType,
        GmosNorthDynamicType,
        VisitRecords.listGmosNorthVisits,
        _.exec.config
      )
    )

  def GmosSouthExecutionConfigType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], ExecutionContext.GmosSouth] =
    ObjectType(
      name        = executionConfigName(Instrument.GmosSouth),
      description = executionConfigDescription(Instrument.GmosSouth),
      interfaces  = List(PossibleInterface.apply[OdbCtx[F], ExecutionContext.GmosSouth](ExecutionConfigType[F])),
      fields      = executionConfigFields(
        Instrument.GmosSouth,
        GmosSouthStaticConfigType,
        GmosSouthDynamicType,
        VisitRecords.listGmosSouthVisits,
        _.exec.config
      )
    )


  def ExecutionType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], Observation.Id] =
    ObjectType(
      name     = "Execution",
      fieldsFn = () => fields(

        Field(
          name        = "datasets",
          fieldType   = DatasetConnectionType[F],
          description = "Datasets associated with the observation".some,
          arguments   = List(
            ArgumentPagingFirst,
            ArgumentPagingCursor
          ),
          resolve     = c =>
            unsafeSelectPageFuture[F, (Step.Id, PosInt), DatasetModel](
              c.pagingCursor("(step-id, index)")(s => DatasetIdCursor.getOption(s).flatMap(DatasetModel.Id.unapply)),
              dm => DatasetIdCursor.reverseGet(dm.id),
              o  => c.ctx.odbRepo.executionEvent.selectDatasetsPageForObservation(c.value, c.pagingFirst, o)
            )
        ),

        Field(
          name        = "events",
          fieldType   = ExecutionEventConnectionType[F],
          description = "Events associated with the observation".some,
          arguments   = List(
            ArgumentPagingFirst,
            ArgumentPagingCursor
          ),
          resolve     = c =>
            unsafeSelectPageFuture[F, ExecutionEvent.Id, ExecutionEventModel](
              c.pagingExecutionEventId,
              (e: ExecutionEventModel) => Cursor.gid[ExecutionEvent.Id].reverseGet(e.id),
              eid => c.ctx.odbRepo.executionEvent.selectEventsPageForObservation(c.value, c.pagingFirst, eid)
            )
        ),

        Field(
          name        = "executionConfig",
          fieldType   = OptionType(ExecutionConfigType[F]),
          description = "Execution config".some,
          resolve     = c =>
            c.unsafeToFuture(
              SequenceComputation.compute[F](c.value, c.ctx.itcClient, c.ctx.odbRepo)
            )
        )

      )
    )

}

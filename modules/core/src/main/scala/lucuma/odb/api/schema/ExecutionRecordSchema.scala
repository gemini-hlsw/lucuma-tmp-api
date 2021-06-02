// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.model.{Observation, Step}
import lucuma.odb.api.repo.OdbRepo

import cats.effect.Effect
import cats.syntax.all._
import eu.timepit.refined.types.all.PosInt
import lucuma.core.util.Gid
import lucuma.odb.api.model.{DatasetModel, ExecutionEvent, ExecutionEventModel}
import monocle.Prism
import sangria.schema._
import scala.util.matching.Regex

object ExecutionRecordSchema {

  import context._
  import DatasetSchema._
  import ExecutionEventSchema._
  import Paging._

  private val PosIntPattern: Regex =
    raw"([1-9a-f][0-9a-f]*)".r

  val datasetCursor: Prism[Cursor, (Step.Id, PosInt)] =
    Prism[Cursor, (Step.Id, PosInt)](
      _.toString.split(',').toList match {
        case List(sid, PosIntPattern(idx)) =>
          (Step.Id.parse(sid),
           PosInt.unapply(java.lang.Integer.parseInt(idx))
          ).bisequence
        case _                             =>
          None
      }
    ) {
      case (sid, idx) =>
        new Cursor(s"${Gid[Step.Id].show(sid)},${idx.value}")
    }

  def ExecutionRecordType[F[_]: Effect]: ObjectType[OdbRepo[F], Observation.Id] =
    ObjectType(
      name     = "ExecutionRecord",
      fieldsFn = () => fields(

        Field(
          name        = "datasets",
          fieldType   = DatasetConnectionType[F],
          description = Some("Datasets associated with the observation"),
          arguments   = List(
            ArgumentPagingFirst,
            ArgumentPagingCursor
          ),
          resolve     = c =>
            unsafeSelectPageFuture[F, (Step.Id, PosInt), DatasetModel](
              c.pagingCursor("(step-id, index)")(datasetCursor.getOption),
              dm => datasetCursor.reverseGet((dm.stepId, dm.index)),
              o  => c.ctx.executionEvent.selectDatasetsForObservation(c.value, c.pagingFirst, o)
            )
        ),

        Field(
          name        = "events",
          fieldType   = ExecutionEventConnectionType[F],
          description = Some("Events associated with the observation"),
          arguments   = List(
            ArgumentPagingFirst,
            ArgumentPagingCursor
          ),
          resolve     = c =>
            unsafeSelectPageFuture[F, ExecutionEvent.Id, ExecutionEventModel](
              c.pagingExecutionEventId,
              (e: ExecutionEventModel) => Cursor.gid[ExecutionEvent.Id].reverseGet(e.id),
              // TODO: here we're going to want the events sorted by timestamp, not GID
              eid => c.ctx.executionEvent.selectEventsForObservation(c.value, c.pagingFirst, eid)
            )
        )

      )
    )

}

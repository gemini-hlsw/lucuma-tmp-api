// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.Order.catsKernelOrderingForOrder
import eu.timepit.refined.cats._
import eu.timepit.refined.types.all.PosInt

import scala.collection.immutable.SortedMap

final case class DatasetTable(
  datasets: SortedMap[Step.Id, SortedMap[PosInt, DatasetModel]]
) {

  def get(id: DatasetModel.Id): Option[DatasetModel] =
    datasets
      .get(id.stepId)
      .flatMap(_.get(id.index))

  def getAll(sid: Step.Id): List[DatasetModel] =
    datasets
      .get(sid)
      .toList
      .flatMap(_.toList.sortBy(_._1).map(_._2))

  def updated(d: DatasetModel): DatasetTable =
    DatasetTable(
      datasets.updated(
        d.id.stepId,
        datasets.get(d.id.stepId).fold(SortedMap(d.id.index -> d)) { m =>
          m.updated(d.id.index, d)
        }
      )
    )

}

object DatasetTable {

  val empty: DatasetTable =
    DatasetTable(SortedMap.empty)

  implicit val EqDatasetTable: Eq[DatasetTable] =
    Eq.by(_.datasets)

}

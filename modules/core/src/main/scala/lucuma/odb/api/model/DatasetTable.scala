// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.Order.catsKernelOrderingForOrder
import cats.syntax.option._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.all.PosInt

import scala.collection.immutable.SortedMap

final case class DatasetTable(
  datasets: SortedMap[Step.Id, SortedMap[PosInt, DatasetModel.Dataset]]
) {

  def get(id: DatasetModel.Id): Option[DatasetModel] =
    datasets
      .get(id.stepId)
      .flatMap(_.get(id.index))
      .map(d => DatasetModel(id, d))

  def allForStep(sid: Step.Id): List[DatasetModel] =
    datasets
      .get(sid)
      .toList
      .flatMap(_.toList.sortBy(_._1).map { case (idx, d) =>
        DatasetModel(DatasetModel.Id(sid, idx), d)
      })

  def updated(d: DatasetModel): DatasetTable =
    DatasetTable(
      datasets.updated(
        d.id.stepId,
        datasets.get(d.id.stepId).fold(SortedMap(d.id.index -> d.dataset)) { m =>
          m.updated(d.id.index, d.dataset)
        }
      )
    )

  def updatedWith(
    id: DatasetModel.Id
  )(
    f: Option[DatasetModel.Dataset] => Option[DatasetModel.Dataset]
  ): DatasetTable =
    DatasetTable(
      datasets.updatedWith(id.stepId) {
        case Some(tab) => tab.updatedWith(id.index)(f).some
        case None      => f(None).map(d => SortedMap(id.index -> d))
      }
    )
}

object DatasetTable {

  val empty: DatasetTable =
    DatasetTable(SortedMap.empty)

  implicit val EqDatasetTable: Eq[DatasetTable] =
    Eq.by(_.datasets)

}

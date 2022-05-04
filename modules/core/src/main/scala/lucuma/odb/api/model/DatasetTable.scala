// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.Order.catsKernelOrderingForOrder
import cats.syntax.eq._
import eu.timepit.refined.cats._
import eu.timepit.refined.auto._
import lucuma.core.model.Observation

import scala.collection.immutable.SortedMap

final case class DatasetTable(
  datasets: SortedMap[DatasetModel.Id, DatasetModel.Dataset]
) {

  def get(id: DatasetModel.Id): Option[DatasetModel] =
    datasets.get(id).map(DatasetModel(id, _))

  def allForStep(oid: Observation.Id, sid: Step.Id): List[DatasetModel] =
    datasets
      .iteratorFrom(DatasetModel.Id(oid, sid, 1))
      .takeWhile { case (id, _) =>
        id.observationId === oid && id.stepId === sid
      }
      .map((DatasetModel.apply _).tupled)
      .toList

  def allForObservation(oid: Observation.Id): List[DatasetModel] =
    datasets
      .iteratorFrom(DatasetModel.Id(oid, Step.Id.Min, 1))
      .takeWhile { case (id, _) => id.observationId === oid }
      .map((DatasetModel.apply _).tupled)
      .toList

  def updated(d: DatasetModel): DatasetTable =
    DatasetTable(datasets.updated(d.id, d.dataset))

  def updatedWith(
    id: DatasetModel.Id
  )(
    f: Option[DatasetModel.Dataset] => Option[DatasetModel.Dataset]
  ): DatasetTable =
    DatasetTable(datasets.updatedWith(id)(f))

}

object DatasetTable {

  val empty: DatasetTable =
    DatasetTable(SortedMap.empty)

  implicit val EqDatasetTable: Eq[DatasetTable] =
    Eq.by(_.datasets)

}

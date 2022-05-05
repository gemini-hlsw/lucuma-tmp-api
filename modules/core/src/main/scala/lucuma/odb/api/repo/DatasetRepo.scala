// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.Functor
import cats.effect.Ref
import cats.syntax.functor._
import cats.syntax.option._
import eu.timepit.refined.types.all.PosInt
import lucuma.core.`enum`.DatasetQaState
import lucuma.core.model.Observation
import lucuma.odb.api.model.{Database, DatasetModel, Step}

sealed trait DatasetRepo[F[_]] {

  def selectDataset(
    id: DatasetModel.Id
  ): F[Option[DatasetModel]]

  def selectDatasets(
    oid: Observation.Id,
    sid: Option[Step.Id],
  ): F[List[DatasetModel]]

  def selectDatasetsPage(
    oid:   Observation.Id,
    sid:   Option[Step.Id],
    count: Option[Int],
    after: Option[DatasetModel.Id]
  ): F[ResultPage[DatasetModel]]

  def markQaState(
    oid:   Observation.Id,
    sid:   Option[Step.Id],
    index: Option[PosInt],
    qa:    DatasetQaState
  ): F[List[DatasetModel]]

}

object DatasetRepo {

  def create[F[_]: Functor](
    databaseRef: Ref[F, Database]
  ): DatasetRepo[F] =

    new DatasetRepo[F] {

      override def selectDataset(
        id: DatasetModel.Id
      ): F[Option[DatasetModel]] =
        databaseRef.get.map { db => db.datasets.select(id) }

      override def selectDatasets(
        oid: Observation.Id,
        sid: Option[Step.Id]
      ): F[List[DatasetModel]] =
        databaseRef.get.map { db => db.datasets.selectAll(oid, sid, None) }

      override def selectDatasetsPage(
        oid:   Observation.Id,
        sid:   Option[Step.Id],
        count: Option[Int],
        after: Option[DatasetModel.Id]
      ): F[ResultPage[DatasetModel]] =
        selectDatasets(oid, sid).map { all =>
          ResultPage.fromSeq(
            all,
            count,
            after,
            _.id
          )
        }

      override def markQaState(
        oid:   Observation.Id,
        sid:   Option[Step.Id],
        index: Option[PosInt],
        qa:    DatasetQaState
      ): F[List[DatasetModel]] =

        databaseRef.modify { db =>
          val dbʹ =
            Database.datasets.modify { t =>
              t.selectAll(oid, sid, index).foldLeft(t) { (tʹ, d) =>
                tʹ.updatedWith(d.id)(_.map(DatasetModel.Dataset.qaState.replace(qa.some)))
              }
            }(db)

          (dbʹ, dbʹ.datasets.selectAll(oid, sid, index))
        }

    }

}

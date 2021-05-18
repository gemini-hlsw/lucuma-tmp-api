// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.core.model.Observation
import lucuma.odb.api.model.{Dataset, DatasetModel, InputError}

import cats.data.{EitherT, State}
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.all._


sealed trait DatasetRepo[F[_]] {

  def selectDataset(
    did: Dataset.Id
  ): F[Option[DatasetModel]]

  def unsafeSelectDataset(
    did: Dataset.Id
  ): F[DatasetModel]

  def selectDatasetsForObservation(
    oid:      Observation.Id,
    count:    Int,
    afterGid: Option[Dataset.Id] = None
  ): F[ResultPage[DatasetModel]]

  def insert(
    dataset: DatasetModel.Create
  ): F[DatasetModel]

}

object DatasetRepo {

  def create[F[_]: Sync](
    tablesRef: Ref[F, Tables]
  ): DatasetRepo[F] =
    new DatasetRepo[F] {

      override def selectDataset(
        did: Dataset.Id
      ): F[Option[DatasetModel]] =
        tablesRef.get.map(Tables.dataset(did).get)

      override def unsafeSelectDataset(
        did: Dataset.Id
      ): F[DatasetModel] =
        selectDataset(did).map(_.getOrElse(sys.error(s"Dataset id '$did' missing")))

      override def selectDatasetsForObservation(
        oid:      Observation.Id,
        count:    Int,
        afterGid: Option[Dataset.Id]
      ): F[ResultPage[DatasetModel]] =
        tablesRef.get.map { tables =>

          ResultPage.select[Dataset.Id, DatasetModel](
            count,
            afterGid,
            tables.datasets.keySet,
            tables.datasets.apply,
            _.observationId === oid
          )

        }

      override def insert(
        newDataset: DatasetModel.Create
      ): F[DatasetModel] =
        EitherT(
          tablesRef.modify { tables =>
            val (tablesʹ, d) = newDataset.create[State[Tables, *], Tables](TableState).run(tables).value
            d.fold(
              err  => (tables, InputError.Exception(err).asLeft),
              dset => (tablesʹ, dset.asRight)
            )
          }
        ).rethrowT
    }

}



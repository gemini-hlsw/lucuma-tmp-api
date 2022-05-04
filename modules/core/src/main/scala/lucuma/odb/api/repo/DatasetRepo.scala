// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.Functor
import cats.effect.Ref
//import cats.syntax.functor._
import cats.syntax.option._
import cats.Order.catsKernelOrderingForOrder
import lucuma.core.`enum`.DatasetQaState
//import lucuma.core.model.Observation
import lucuma.odb.api.model.{Database, DatasetModel}

sealed trait DatasetRepo[F[_]] {

//  def datasetsForObservation(
//    oid: Observation.Id
//  ): F[List[DatasetModel]]

  def markQaState(
    qa:  DatasetQaState,
    ids: List[DatasetModel.Id],
  ): F[List[DatasetModel]]

}

object DatasetRepo {

  def create[F[_]: Functor](
    databaseRef: Ref[F, Database]
  ): DatasetRepo[F] =

    new DatasetRepo[F] {

//      override def datasetsForObservation(
//        oid: Observation.Id
//      ): F[List[DatasetModel]] =
//
//        databaseRef.get.map { db =>
//          db.datasets.allForStep()
//        }

      override def markQaState(
        qa:  DatasetQaState,
        ids: List[DatasetModel.Id]
      ): F[List[DatasetModel]] =

        databaseRef.modify { db => (
          Database.datasets.modify { t =>
            ids.foldLeft(t) { (tʹ, id) =>
              tʹ.updatedWith(id)(_.map(DatasetModel.Dataset.qaState.replace(qa.some)))
            }
          }(db),

          ids.foldLeft(List.empty[DatasetModel]) { (lst, id) =>
            db.datasets.get(id).fold(lst)(_ :: lst)
          }.sortBy(_.id)
        )}

    }

}

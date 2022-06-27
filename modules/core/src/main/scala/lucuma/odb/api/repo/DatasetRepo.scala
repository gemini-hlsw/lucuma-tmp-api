// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.MonadError
import cats.effect.Ref
import cats.syntax.flatMap._
import cats.syntax.functor._
//import cats.syntax.all._
import eu.timepit.refined.types.all.NonNegInt
import lucuma.core.model.Observation
import lucuma.odb.api.model.query.{SizeLimitedResult, WherePredicate}
import lucuma.odb.api.model.{Database, DatasetModel, Step}
import lucuma.odb.api.model.syntax.databasestate._
import lucuma.odb.api.model.syntax.eitherinput._

sealed trait DatasetRepo[F[_]] {

  def selectDataset(
    id: DatasetModel.Id
  ): F[Option[DatasetModel]]

  def selectWhere(
    where:  WherePredicate[DatasetModel],
    offset: Option[DatasetModel.Id],
    limit:  Option[NonNegInt]
  ): F[SizeLimitedResult[DatasetModel]]

  def selectDatasets(
    oid: Observation.Id,
    sid: Option[Step.Id],
  ): F[List[DatasetModel]]

  def edit(
    editInput: DatasetModel.EditInput
  ): F[DatasetModel.EditResult]

}

object DatasetRepo {

  def create[F[_]](
    databaseRef: Ref[F, Database]
  )(implicit ev: MonadError[F, Throwable]): DatasetRepo[F] =

    new DatasetRepo[F] {

      override def selectDataset(
        id: DatasetModel.Id
      ): F[Option[DatasetModel]] =
        databaseRef.get.map { db => db.datasets.select(id) }

      override def selectWhere(
        where:  WherePredicate[DatasetModel],
        offset: Option[DatasetModel.Id],
        limit:  Option[NonNegInt]
      ): F[SizeLimitedResult[DatasetModel]] = {

        databaseRef.get.map { tables =>
          val all     = tables.datasets.datasets
          val off     = offset.fold(all.iterator)(all.iteratorFrom).to(LazyList).map((DatasetModel.apply _).tupled)
          val matches = off.filter(where.matches)
          val lim     = limit.map(_.value).getOrElse(Int.MaxValue)

          val (result, rest) = matches.splitAt(lim)

          SizeLimitedResult.Select(
            result.toList,
            rest.nonEmpty
          )
        }
      }


      override def selectDatasets(
        oid: Observation.Id,
        sid: Option[Step.Id]
      ): F[List[DatasetModel]] =
        databaseRef.get.map { db => db.datasets.selectAll(oid, sid, None) }

      override def edit(
        editInput: DatasetModel.EditInput
      ): F[DatasetModel.EditResult] =
        databaseRef
          .modifyState(editInput.editor.flipF)
          .flatMap(_.liftTo[F])
          .map(ds => DatasetModel.EditResult(ds))

    }

}

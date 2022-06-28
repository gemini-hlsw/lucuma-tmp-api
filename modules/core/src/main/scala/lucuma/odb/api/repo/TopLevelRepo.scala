// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{Database, TopLevelModel}
import lucuma.odb.api.model.syntax.toplevel._
import lucuma.core.util.Gid
import cats._
import cats.effect.Ref
import cats.kernel.BoundedEnumerable
import cats.syntax.all._
import eu.timepit.refined.types.all.NonNegInt
import monocle.Lens
import monocle.function.At
import lucuma.core.optics.state.all._
import lucuma.odb.api.model.query.{SizeLimitedResult, WherePredicate}

import scala.collection.immutable.SortedMap


trait TopLevelRepo[F[_], I, T] {

  def nextId: F[I]

  def select(id: I, includeDeleted: Boolean = false): F[Option[T]]

  def unsafeSelect(id: I, includeDeleted: Boolean = false): F[T]

  def selectAll(
    includeDeleted: Boolean = false
  ): F[List[T]]

  def selectWhere(
    where:  WherePredicate[T],
    offset: Option[I],
    limit:  Option[NonNegInt]
  ): F[SizeLimitedResult[T]]

}

abstract class TopLevelRepoBase[F[_], I: Gid, T: TopLevelModel[I, *]](
  databaseRef:  Ref[F, Database],
  idLens:       Lens[Database, I],
  mapLens:      Lens[Database, SortedMap[I, T]]
)(implicit M: MonadError[F, Throwable]) extends TopLevelRepo[F, I, T] {

  def nextId: F[I] =
    databaseRef.modifyState(idLens.mod(BoundedEnumerable[I].cycleNext))

  def deletionFilter[FF[_]: FunctorFilter](includeDeleted: Boolean)(ff: FF[T]): FF[T] =
    ff.filter(t => includeDeleted || t.isPresent)

  def focusOn(id: I): Lens[Database, Option[T]] =
    mapLens.andThen(At.at(id))

  def select(id: I, includeDeleted: Boolean = false): F[Option[T]] =
    selectUnconditional(id).map(deletionFilter(includeDeleted))

  def unsafeSelect(id: I, includeDeleted: Boolean = false): F[T] =
    select(id, includeDeleted).flatMap {
      case None    => ExecutionException.missingReference[F, I, T](id)
      case Some(t) => t.pure[F]
    }

  def selectUnconditional(id: I): F[Option[T]] =
    databaseRef.get.map(focusOn(id).get)

  override def selectAll(
    includeDeleted: Boolean
  ): F[List[T]] =
    databaseRef.get.map { tables =>
      val all = mapLens.get(tables).values
      (if (includeDeleted) all else all.filter(_.isPresent)).toList
    }

  override def selectWhere(
    where:  WherePredicate[T],
    offset: Option[I],
    limit:  Option[NonNegInt]
  ): F[SizeLimitedResult[T]] =
    databaseRef.get.map { tables =>
      val all     = mapLens.get(tables)
      val off     = offset.fold(all.iterator)(all.iteratorFrom).to(LazyList).map(_._2)
      val matches = off.filter(where.matches)
      SizeLimitedResult.Select.fromAll(matches.toList, limit)
    }

}

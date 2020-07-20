// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.syntax.all._
import lucuma.odb.api.model.{Editor, Event, Existence, InputError, TopLevel}
import lucuma.core.util.Gid
import cats.{FunctorFilter, Monad, MonadError}
import cats.data.{EitherNec, State}
import cats.effect.concurrent.Ref
import cats.implicits._
import cats.kernel.BoundedEnumerable
import monocle.Lens
import monocle.function.At
import monocle.state.all._

import scala.Function.const
import scala.collection.immutable.SortedMap

trait TopLevelRepo[F[_], I, T] {

  def nextId: F[I]

  def select(id: I, includeDeleted: Boolean = false): F[Option[T]]

  def unsafeSelect(id: I, includeDeleted: Boolean = false): F[T]

  def selectAll(includeDeleted: Boolean = false): F[List[T]]

  def selectAllWhere(f: Tables => T => Boolean, includeDeleted: Boolean = false): F[List[T]]

  def edit(editor: Editor[I, T]): F[Option[T]]

  def delete(id: I): F[Option[T]]

  def undelete(id: I): F[Option[T]]

}

/**
 *
 */
abstract class TopLevelRepoBase[F[_], I: Gid, T: TopLevel[I, ?]](
  tablesRef:    Ref[F, Tables],
  eventService: EventService[F],
  idLens:       Lens[Tables, I],
  mapLens:      Lens[Tables, SortedMap[I, T]],
  created:      T => Long => Event.Created[T],
  edited:       (T, T) => Long => Event.Edited[T]
)(implicit F: Monad[F], M: MonadError[F, Throwable]) extends TopLevelRepo[F, I, T] {

  def nextId: F[I] =
    tablesRef.modifyState(idLens.mod(BoundedEnumerable[I].cycleNext))

  def deletionFilter[FF[_]: FunctorFilter](includeDeleted: Boolean)(ff: FF[T]): FF[T] =
    ff.filter(t => includeDeleted || t.isPresent)

  def focusOn(id: I): Lens[Tables, Option[T]] =
    mapLens ^|-> At.at(id)

  def select(id: I, includeDeleted: Boolean = false): F[Option[T]] =
    selectUnconditional(id).map(deletionFilter(includeDeleted))

  def unsafeSelect(id: I, includeDeleted: Boolean = false): F[T] =
    select(id, includeDeleted).flatMap {
      case None    => ExecutionException.missingReference[F, I, T](id)
      case Some(t) => t.pure[F]
    }

  def selectUnconditional(id: I): F[Option[T]] =
    tablesRef.get.map(focusOn(id).get)

  def selectAll(includeDeleted: Boolean = false): F[List[T]] =
    selectAllWhere(const(const(true)), includeDeleted)

  def selectAllWhere(f: Tables => T => Boolean, includeDeleted: Boolean = false): F[List[T]] =
    selectAllWhereUnconditional(f).map(deletionFilter(includeDeleted))

  def selectAllWhereUnconditional(f: Tables => T => Boolean): F[List[T]] =
    tablesRef.get.map { tables =>
      mapLens.get(tables).values.toList.filter(f(tables))
    }

  def createAndInsert(f: I => T): State[Tables, T] =
    for {
      i <- idLens.mod(BoundedEnumerable[I].cycleNext)
      t  = f(i)
      _ <- mapLens.mod(_ + (i -> t))
    } yield t

  def modify(f: Tables => (Tables, EitherNec[InputError, T])): F[T] = {
    val ft: F[T] = tablesRef.modify(f).flatMap {
      case Left(err) => M.raiseError(InputError.Exception(err))
      case Right(t)  => M.pure(t)
    }

    for {
      t <- ft
      _ <- eventService.publish(created(t))
    } yield t
  }

  def edit(editor: Editor[I, T]): F[Option[T]] = {

    val doUpdate = tablesRef.modify { oldTables =>
      val lens      = focusOn(editor.id)
      val oldT      = lens.get(oldTables)
      val newTables = lens.modify(_.map(editor.edit))(oldTables)
      val newT      = lens.get(newTables)
      (newTables, (oldT, newT).mapN((o, n) => (o, n)))
    }

    for {
      u <- doUpdate
      _ <- u.fold(F.unit) { case (o, n) =>
             eventService.publish(edited(o, n))
           }
    } yield u.map(_._2)

  }

  private def setExistence(id: I, newState: Existence): F[Option[T]] =
    edit(TopLevel[I, T].existenceEditor(id, newState))

  def delete(id: I): F[Option[T]] =
    setExistence(id, Existence.Deleted)

  def undelete(id: I): F[Option[T]] =
    setExistence(id, Existence.Present)

}

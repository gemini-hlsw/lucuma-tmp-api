// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{Event, Existence, InputError, TopLevelModel}
import lucuma.odb.api.model.syntax.toplevel._
import lucuma.core.util.Gid
import cats.{FunctorFilter, Monad, MonadError}
import cats.data.{EitherNec, State}
import cats.effect.concurrent.Ref
import cats.syntax.apply._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.functorFilter._
import cats.syntax.option._
import cats.kernel.BoundedEnumerable
import monocle.Lens
import monocle.function.At
import monocle.state.all._

import Function.unlift
import scala.Function.const
import scala.collection.immutable.SortedMap

trait TopLevelRepo[F[_], I, T] {

  def nextId: F[I]

  def select(id: I, includeDeleted: Boolean = false): F[Option[T]]

  def unsafeSelect(id: I, includeDeleted: Boolean = false): F[T]

  def selectAll(includeDeleted: Boolean = false): F[List[T]]

  def selectAllWhere(f: Tables => T => Boolean, includeDeleted: Boolean = false): F[List[T]]

  def edit(id: I, s: State[T, Unit]): F[Option[T]]

  def editSub[U <: T](id: I, s: State[U, Unit])(f: PartialFunction[T, U]): F[Option[U]]

  def delete(id: I): F[Option[T]]

  def undelete(id: I): F[Option[T]]

}

/**
 *
 */
abstract class TopLevelRepoBase[F[_], I: Gid, T: TopLevelModel[I, ?]](
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

  def createAndInsert[U <: T](f: I => U): State[Tables, U] =
    for {
      i <- idLens.mod(BoundedEnumerable[I].cycleNext)
      t  = f(i)
      _ <- mapLens.mod(_ + (i -> t))
    } yield t

  def modify[U <: T](f: Tables => (Tables, EitherNec[InputError, U])): F[U] = {
    val fu: F[U] = tablesRef.modify(f).flatMap {
      case Left(err) => M.raiseError(InputError.Exception(err))
      case Right(u)  => M.pure(u)
    }

    for {
      u <- fu
      _ <- eventService.publish(created(u))
    } yield u
  }

  override def editSub[U <: T](id: I, s: State[U, Unit])(f: PartialFunction[T, U]): F[Option[U]] = {

    val doUpdate: F[Option[(U, U)]] =
      tablesRef.modify { oldTables =>
        val lensT = focusOn(id)

        val lens: Lens[Tables, Option[U]] =
          Lens[Tables, Option[U]](lensT.get(_).flatMap(f.unapply))(ou => lensT.set(ou))

        val oldU      = lens.get(oldTables)
        val newTables = lens.modify(_.map(u => s.runS(u).value))(oldTables)
        val newU      = lens.get(newTables)

        (newTables, (oldU, newU).mapN((o, n) => (o, n)))
      }

    for {
      u <- doUpdate
      _ <- u.fold(F.unit) { case (o, n) => eventService.publish(edited(o, n)) }
    } yield u.map(_._2)

  }

  override def edit(id: I, s: State[T, Unit]): F[Option[T]] =
    editSub[T](id, s)(unlift(_.some))

  private def setExistence(id: I, newState: Existence): F[Option[T]] =
    edit(id, TopLevelModel[I, T].existenceEditor(newState))

  def delete(id: I): F[Option[T]] =
    setExistence(id, Existence.Deleted)

  def undelete(id: I): F[Option[T]] =
    setExistence(id, Existence.Present)

}

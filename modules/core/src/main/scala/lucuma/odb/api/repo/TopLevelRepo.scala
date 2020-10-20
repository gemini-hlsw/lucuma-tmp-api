// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{Editor, Event, Existence, InputError, TopLevelModel, ValidatedInput}
import lucuma.odb.api.model.syntax.toplevel._
import lucuma.odb.api.model.syntax.validatedinput._
import lucuma.core.util.Gid
import cats.{FunctorFilter, Monad, MonadError}
import cats.data._
import cats.effect.concurrent.Ref
import cats.syntax.apply._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.functorFilter._
import cats.syntax.option._
import cats.syntax.validated._
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

  /**
   * Edits the top-level item identified by the given `Editor`.
   * @param editor editor instance
   * @return updated item
   */
  def edit(editor: Editor[I, T]): F[T] =
    edit(editor.id, editor.editor, _ => Nil)

  /**
   * Edits the top-level item identified by the given id and editor
   *
   * @param id id of the top level item
   * @param editor state program that edits the associated top-level item, or else
   *          any input validation errors
   * @param checks additional checks that require accessing database tables
   * @return updated item, but raises an error and does nothing if any checks
   *         fail
   */
  def edit(
    id:     I,
    editor: ValidatedInput[State[T, Unit]],
    checks: Tables => List[InputError]
  ): F[T] =
    editSub[T](id, editor, checks)(unlift(_.some))

  /**
   * Edits a top-level item identified by the given id, assuming it is of type
   * `U <: T`.
   *
   * @param id id of the top level item (of type T)
   * @param editor state program that edits the associated top-level item, or else
   *               any input validation errors
   * @param checks additional checks that require accessing database tables
   * @param f a partial function defined when the item is of type U
   * @return updated item, but raises an error and does nothing if any checks
   *         fail
   */
  def editSub[U <: T](
    id:     I,
    editor: ValidatedInput[State[U, Unit]],
    checks: Tables => List[InputError]
  )(
    f: PartialFunction[T, U]
  ): F[U]

  def delete(id: I): F[T]

  def undelete(id: I): F[T]

}

/**
 *
 */
abstract class TopLevelRepoBase[F[_]: Monad, I: Gid, T: TopLevelModel[I, ?]](
  tablesRef:    Ref[F, Tables],
  eventService: EventService[F],
  idLens:       Lens[Tables, I],
  mapLens:      Lens[Tables, SortedMap[I, T]],
  created:      T => Long => Event.Created[T],
  edited:       (T, T) => Long => Event.Edited[T]
)(implicit M: MonadError[F, Throwable]) extends TopLevelRepo[F, I, T] {

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

  override def editSub[U <: T](
    id:     I,
    editor: ValidatedInput[State[U, Unit]],
    checks: Tables => List[InputError]
  )(
    f: PartialFunction[T, U]
  ): F[U] = {

    val lensT = focusOn(id)

    val lens: Lens[Tables, Option[U]] =
      Lens[Tables, Option[U]](lensT.get(_).flatMap(f.unapply))(ou => lensT.set(ou))

    val doUpdate: F[(U, U)] =
      tablesRef.modify { oldTables =>

        val item   = lens.get(oldTables).toValidNec(InputError.missingReference(s"id", Gid[I].show(id)))
        val errors = NonEmptyChain.fromSeq(checks(oldTables))
        val result = (item, editor, errors.toInvalid(())).mapN { (oldU, state, _) =>
          (oldU, state.runS(oldU).value)
        }

        val tables = result.fold(_ => oldTables, { case (_, newU) => lens.set(Some(newU))(oldTables) })

        (tables, result)
      }.flatMap(_.liftTo[F])

    for {
      u <- doUpdate
      (o, n) = u
      _ <- eventService.publish(edited(o, n))
    } yield n

  }

  private def setExistence(id: I, newState: Existence): F[T] =
    edit(id, TopLevelModel[I, T].existenceEditor(newState).validNec, _ => Nil)

  def delete(id: I): F[T] =
    setExistence(id, Existence.Deleted)

  def undelete(id: I): F[T] =
    setExistence(id, Existence.Present)

}

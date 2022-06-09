// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{Database, Event, Existence, InputError, TopLevelModel, ValidatedInput}
import lucuma.odb.api.model.syntax.toplevel._
import lucuma.odb.api.model.syntax.validatedinput._
import lucuma.core.util.Gid
import cats._
import cats.data._
import cats.effect.Ref
import cats.kernel.BoundedEnumerable
import cats.syntax.all._
import eu.timepit.refined.types.all.NonNegInt
import monocle.Lens
import monocle.function.At
import lucuma.core.optics.state.all._
import lucuma.odb.api.model.query.{SelectResult, WherePredicate}

import scala.collection.immutable.SortedMap


trait TopLevelRepo[F[_], I, T] {

  def nextId: F[I]

  def select(id: I, includeDeleted: Boolean = false): F[Option[T]]

  def unsafeSelect(id: I, includeDeleted: Boolean = false): F[T]

  def selectAll(
    includeDeleted: Boolean = false
  ): F[List[T]]

  def selectWhere(
    where: Option[WherePredicate[T]],
    limit: NonNegInt
  ): F[SelectResult[T]]

  def selectPage(
    count:          Option[Int] = None,
    afterGid:       Option[I]   = None,
    includeDeleted: Boolean     = false
  ): F[ResultPage[T]] =
    selectPageFiltered(count, afterGid, includeDeleted) { Function.const(true) }

  def selectPageFiltered(
    count:          Option[Int] = None,
    afterGid:       Option[I]   = None,
    includeDeleted: Boolean     = false
  )(
    predicate: T => Boolean
  ): F[ResultPage[T]]

  def selectPageFromIds(
    count:          Option[Int] = None,
    afterGid:       Option[I]   = None,
    includeDeleted: Boolean     = false
  )(
    ids: Database => scala.collection.immutable.SortedSet[I]
  ): F[ResultPage[T]]

  /**
   * Edits the top-level item identified by the given id and editor
   *
   * @param id id of the top level item
   * @param editor state program that edits the associated top-level item, or else
   *          any input validation errors
   * @return updated item, but raises an error and does nothing if any checks
   *         fail
   */
  def edit(
    id:     I,
    editor: ValidatedInput[State[T, Unit]]
  ): F[T]

  def deleteOne(id: I): F[T]

  def undeleteOne(id: I): F[T]

}

abstract class TopLevelRepoBase[F[_], I: Gid, T: TopLevelModel[I, *]: Eq](
  databaseRef:  Ref[F, Database],
  eventService: EventService[F],
  idLens:       Lens[Database, I],
  mapLens:      Lens[Database, SortedMap[I, T]],
  edited:       (Event.EditType, T) => Long => Event.Edit[T]
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
    selectPage(
      count          = Some(Integer.MAX_VALUE),
      afterGid       = None,
      includeDeleted = includeDeleted
    ).map(_.nodes)

  override def selectWhere(
    where: Option[WherePredicate[T]],
    limit: NonNegInt
  ): F[SelectResult[T]] =
    databaseRef.get.map { tables =>
      val all     = mapLens.get(tables)
      val matches = where.fold(all.values) { w => all.values.filter(w.matches) }

      SelectResult.Standard(
        matches.take(limit.value).toList,
        NonNegInt.unsafeFrom(matches.size)
      )
    }

  def selectPageFiltered(
    count:          Option[Int],
    afterGid:       Option[I],
    includeDeleted: Boolean
  )(
    predicate: T => Boolean
  ): F[ResultPage[T]] =

    databaseRef.get.map { tables =>
      val all = mapLens.get(tables)

      ResultPage.select(
        count,
        afterGid,
        all.keySet,
        all.apply,
        t => (includeDeleted || t.isPresent) && predicate(t)
      )
    }

  def selectPageFromIds(
    count:          Option[Int],
    afterGid:       Option[I],
    includeDeleted: Boolean
  )(
    ids: Database => scala.collection.immutable.SortedSet[I]
  ): F[ResultPage[T]] =

    databaseRef.get.map { tables =>
      ResultPage.select(
        count,
        afterGid,
        ids(tables),
        mapLens.get(tables).apply,
        t => includeDeleted || t.isPresent
      )
    }

  def constructAndPublish[U <: T](
    cons: Database => ValidatedInput[State[Database, U]]
  ): F[U] = {
    val fu = EitherT(
      databaseRef.modify { tables =>
        cons(tables).fold(
          err => (tables, InputError.Exception(err).asLeft[U]),
          _.run(tables).value.map(_.asRight)
        )
      }
    ).rethrowT

    for {
      u <- fu
      _ <- eventService.publish(edited(Event.EditType.Created, u))
    } yield u
  }

  override def edit(
    id:     I,
    editor: ValidatedInput[State[T, Unit]],
  ): F[T] = {

    val lens = focusOn(id)

    val doUpdate: F[Either[T, T]] =
      databaseRef.modify { oldTables =>

        val item   = lens.get(oldTables).toValidNec(InputError.missingReference("id", Gid[I].show(id)))
        val result = (item, editor).mapN { (oldT, state) =>
          val newT = state.runS(oldT).value
          Either.cond(oldT =!= newT, newT, oldT) // Right => updated, Left => no update
        }

        val tables = result.toOption.flatMap(_.toOption)
                       .map(t => lens.replace(t.some)(oldTables))
                       .getOrElse(oldTables)

        (tables, result)
      }.flatMap(_.liftTo[F])

    for {
      e <- doUpdate
      _ <- e.fold(_ => M.unit, t => eventService.publish(edited(Event.EditType.Updated, t)))
    } yield e.merge

  }

  private def setExistence(id: I, newState: Existence): F[T] =
    edit(id, TopLevelModel[I, T].existenceEditor(newState).validNec)

  def deleteOne(id: I): F[T] =
    setExistence(id, Existence.Deleted)

  def undeleteOne(id: I): F[T] =
    setExistence(id, Existence.Present)

}

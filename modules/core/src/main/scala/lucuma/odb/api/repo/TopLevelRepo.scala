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
      val lim     = limit.map(_.value).getOrElse(Int.MaxValue)

      val (result, rest) = matches.splitAt(lim)

      SizeLimitedResult.Select(
        result.toList,
        rest.nonEmpty
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

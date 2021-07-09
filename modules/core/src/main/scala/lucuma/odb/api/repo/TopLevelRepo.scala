// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{Event, Existence, InputError, Sharing, TopLevelModel, ValidatedInput}
import lucuma.odb.api.model.syntax.toplevel._
import lucuma.odb.api.model.syntax.validatedinput._
import lucuma.core.util.Gid

import cats._
import cats.data._
import cats.effect.Ref
import cats.kernel.BoundedEnumerable
import cats.syntax.all._
import monocle.Lens
import monocle.function.At
import lucuma.core.optics.state.all._

import scala.collection.immutable.SortedMap


trait TopLevelRepo[F[_], I, T] {

  def nextId: F[I]

  def select(id: I, includeDeleted: Boolean = false): F[Option[T]]

  def unsafeSelect(id: I, includeDeleted: Boolean = false): F[T]

  def selectAll(
    includeDeleted: Boolean = false
  ): F[List[T]]

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
    ids: Tables => scala.collection.immutable.SortedSet[I]
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

  def delete(id: I): F[T]

  def undelete(id: I): F[T]

}

abstract class TopLevelRepoBase[F[_], I: Gid, T: TopLevelModel[I, *]: Eq](
  tablesRef:    Ref[F, Tables],
  eventService: EventService[F],
  idLens:       Lens[Tables, I],
  mapLens:      Lens[Tables, SortedMap[I, T]],
  edited:       (Event.EditType, T) => Long => Event.Edit[T]
)(implicit M: MonadError[F, Throwable]) extends TopLevelRepo[F, I, T] {

  def nextId: F[I] =
    tablesRef.modifyState(idLens.mod(BoundedEnumerable[I].cycleNext))

  def deletionFilter[FF[_]: FunctorFilter](includeDeleted: Boolean)(ff: FF[T]): FF[T] =
    ff.filter(t => includeDeleted || t.isPresent)

  def focusOn(id: I): Lens[Tables, Option[T]] =
    mapLens.andThen(At.at(id))

  def select(id: I, includeDeleted: Boolean = false): F[Option[T]] =
    selectUnconditional(id).map(deletionFilter(includeDeleted))

  def unsafeSelect(id: I, includeDeleted: Boolean = false): F[T] =
    select(id, includeDeleted).flatMap {
      case None    => ExecutionException.missingReference[F, I, T](id)
      case Some(t) => t.pure[F]
    }

  def selectUnconditional(id: I): F[Option[T]] =
    tablesRef.get.map(focusOn(id).get)

  override def selectAll(
    includeDeleted: Boolean
  ): F[List[T]] =
    selectPage(
      count          = Some(Integer.MAX_VALUE),
      afterGid       = None,
      includeDeleted = includeDeleted
    ).map(_.nodes)

  def selectPageFiltered(
    count:          Option[Int],
    afterGid:       Option[I],
    includeDeleted: Boolean
  )(
    predicate: T => Boolean
  ): F[ResultPage[T]] =

    tablesRef.get.map { tables =>
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
    ids: Tables => scala.collection.immutable.SortedSet[I]
  ): F[ResultPage[T]] =

    tablesRef.get.map { tables =>
      ResultPage.select(
        count,
        afterGid,
        ids(tables),
        mapLens.get(tables).apply,
        t => includeDeleted || t.isPresent
      )
    }

  def constructAndPublish[U <: T](
    cons: Tables => ValidatedInput[State[Tables, U]]
  ): F[U] = {
    val fu = EitherT(
      tablesRef.modify { tables =>
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
      tablesRef.modify { oldTables =>

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

  def delete(id: I): F[T] =
    setExistence(id, Existence.Deleted)

  def undelete(id: I): F[T] =
    setExistence(id, Existence.Present)

  private def share[G[_] : Traverse, J, M](
    name:    String,
    one:     I,
    many:    G[J],
    findM:   J => State[Tables, ValidatedInput[M]],
    editedM: M => Long => Event.Edit[M]
  )(
    update:  ValidatedInput[(T, G[M])] => State[Tables, ValidatedInput[Unit]],
  ): F[T] = {

    val link = tablesRef.modifyState {
      for {
        vo  <- focusOn(one).st.map(_.toValidNec(InputError.missingReference(name, Gid[I].show(one))))
        vm  <- many.traverse(findM).map(_.sequence)
        vtm  = (vo, vm).tupled
        r   <- update(vtm)
      } yield (vtm, r)
    }

    for {
      tm      <- link.flatMap(_._1.liftTo[F])
      _       <- link.flatMap(_._2.liftTo[F])  // this would be a duplicate error if tm was a failure
      (t, gm)  = tm
      _       <- eventService.publish(edited(Event.EditType.Updated, t)) // publish one
      _       <- gm.traverse_(m => eventService.publish(editedM(m)))     // publish many
    } yield t
  }

  protected def shareLeft[J, M](
    name:     String,
    input:    Sharing[I, J],
    findM:    J => State[Tables, ValidatedInput[M]],
    linkLens: Lens[Tables, ManyToMany[J, I]],
    editedM:  M => Long => Event.Edit[M]
 )(
    update:   (ManyToMany[J, I], IterableOnce[(J, I)]) => ManyToMany[J, I]
 ): F[T] =
    share(name, input.one, input.many, findM, editedM) { vtm =>
      vtm.traverse_ { _ => linkLens.mod_(links => update(links, input.tupleRight)) }.map(_.validNec[InputError].void)
    }

  protected def shareRight[J, M](
    name:     String,
    input:    Sharing[I, J],
    findM:    J => State[Tables, ValidatedInput[M]],
    linkLens: Lens[Tables, ManyToMany[I, J]],
    editedM:  M => Long => Event.Edit[M]
  )(
    update:   (ManyToMany[I, J], IterableOnce[(I, J)]) => ManyToMany[I, J]
  ): F[T] =
    share(name, input.one, input.many, findM, editedM) { vtm =>
      vtm.traverse_ { _ => linkLens.mod_(links => update(links, input.tupleLeft)) }.map(_.validNec[InputError].void)
    }

  protected def shareWithOne[J, M](
    name: String,
    id: I,
    oneId: J,
    findM: J => State[Tables, ValidatedInput[M]],
    linkLens: Lens[Tables, OneToMany[J, I]],
    editedM: M => Long => Event.Edit[M]
  )(
    update: (OneToMany[J, I], (J, I)) => OneToMany[J, I]
  ): F[T] =
    share[Id, J, M](name, id, oneId, findM, editedM) { vtm =>
      vtm.traverse_ { _ => linkLens.mod_(links => update(links, (oneId, id))) }.map(_.validNec[InputError].void)
    }

  protected def shareOneWithMany[J, M](
    name:     String,
    input:    Sharing[I, J],
    findM:    J => State[Tables, ValidatedInput[M]],
    linkLens: Lens[Tables, OneToMany[I, J]],
    editedM:  M => Long => Event.Edit[M]
  )(
    update:   (OneToMany[I, J], IterableOnce[(I, J)]) => OneToMany[I, J]
  ): F[T] =
    share(name, input.one, input.many, findM, editedM) { vtm =>
      vtm.traverse_ { _ => linkLens.mod_(links => update(links, input.tupleLeft)) }.map(_.validNec[InputError].void)
    }

}

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.util.Gid
import cats.data.State
import cats.kernel.BoundedEnumerable
import cats.syntax.all._
import monocle.Lens
import monocle.state.all._

import scala.collection.immutable.SortedMap

final class Store[T, I: Gid, M](
  name:      String,
  idLens:    Lens[T, I],
  modelLens: Lens[T, SortedMap[I, M]]
) {

  def isDefinedAt(id: I): State[T, Boolean] =
    modelLens.st.map(_.isDefinedAt(id))

  def isEmptyAt(id: I): State[T, Boolean] =
    isDefinedAt(id).map(r => !r)

  val cycleNextUnused: State[T, I] = {
    val unused: State[T, Boolean] =
      for {
        i <- idLens.extract
        b <- isEmptyAt(i)
      } yield b

    for {
      _ <- idLens.mod(BoundedEnumerable[I].cycleNext).untilM_(unused)
      i <- idLens.extract
    } yield i
  }

  private def alreadyDefined[A](i: I): State[T, ValidatedInput[A]] =
    State.pure[T, ValidatedInput[A]](
      InputError
        .fromMessage(s"$name `${Gid[I].show(i)}` is already defined")
        .invalidNec[A]
    )

  def getUnusedId(suggestion: Option[I]): State[T, ValidatedInput[I]] =
    suggestion.fold(cycleNextUnused.map(_.validNec[InputError])) { id =>
      isEmptyAt(id).ifM(
        State.pure[T, ValidatedInput[I]](id.validNec[InputError]),
        alreadyDefined[I](id)
      )
    }

  def lookup(id: I): State[T, ValidatedInput[M]] =
    modelLens
      .st
      .map(_.get(id).toValidNec(InputError.missingReference(name, Gid[I].show(id))))

  def orError(id: I): State[T, M] =
    lookup(id).map(_.valueOr(nec => throw InputError.Exception(nec)))

  def optionalLookup(id: Option[I]): State[T, ValidatedInput[Option[M]]] =
    id.traverse(lookup).map(_.sequence)

  def save(i: I, m: M): State[T, ValidatedInput[Unit]] =
    isDefinedAt(i).ifM(
      alreadyDefined[Unit](i),
      modelLens.mod_(_ + (i -> m)).map(_.validNec[InputError])
    )

  def saveValid(vim: ValidatedInput[M])(id: M => I): State[T, ValidatedInput[Unit]] =
    vim.fold(
      nec => State.pure[T, ValidatedInput[Unit]](nec.invalid[Unit]),
      m   => save(id(m), m)
    )
}

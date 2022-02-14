// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.data.StateT
import cats.kernel.BoundedEnumerable
import cats.syntax.all._
import lucuma.core.util.Gid

trait TableModify[K, V] extends TableInspect[K, V] {

  def cycleNextUnused(implicit G: Gid[K]): StateT[EitherInput, Table[K, V], K] = {
    val unused: StateT[EitherInput, Table[K, V], Boolean] =
      for {
        k <- StateT.inspect[EitherInput, Table[K, V], K](_.lastKey)
        b <- isEmptyAt(k)
      } yield b

    for {
      _ <- StateT.modify[EitherInput, Table[K, V]](Table.lastKey[K, V].modify(BoundedEnumerable[K].cycleNext)).untilM_(unused)
      k <- StateT.inspect[EitherInput, Table[K, V], K](_.lastKey)
    } yield k
  }

  def alreadyDefined[A](k: K)(implicit G: Gid[K]): StateT[EitherInput, Table[K, V], A] =
    StateT.apply{ _ =>
      InputError.fromMessage(s"${G.show(k)} is already defined").leftNec[(Table[K, V], A)]
    }

  def getUnusedKey(suggestion: Option[K])(implicit G: Gid[K]): StateT[EitherInput, Table[K, V], K] =
    suggestion.fold(cycleNextUnused) { k =>
      isEmptyAt(k).ifM(StateT.pure(k), alreadyDefined[K](k))
    }

  def saveNew(k: K, v: V)(implicit G: Gid[K]): StateT[EitherInput, Table[K, V], Unit] =
    isDefinedAt(k).ifM(alreadyDefined[Unit](k), update(k, v))

  def saveNewIfValid(v: ValidatedInput[V])(key: V => K)(implicit G: Gid[K]): StateT[EitherInput, Table[K, V], Unit] =
    v.fold(
      nec => StateT.setF(nec.asLeft[Table[K, V]]),
      v   => saveNew(key(v), v)
    )

  def update(k: K, v: V): StateT[EitherInput, Table[K, V], Unit] =
    StateT.modify(Table.rows.modify(_ + (k -> v)))

  def delete(k: K): StateT[EitherInput, Table[K, V], Unit] =
    StateT.modify(Table.rows.modify(_ - k))
}

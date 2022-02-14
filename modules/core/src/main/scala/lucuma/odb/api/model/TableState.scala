// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.data.{NonEmptyChain, StateT}
import cats.kernel.BoundedEnumerable
import cats.syntax.parallel._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.monad._
import lucuma.core.util.Gid

trait TableState {

  def isDefinedAt[K, V](k: K): StateT[EitherInput, Table[K, V], Boolean] =
    StateT.inspect(_.rows.isDefinedAt(k))

  def isEmptyAt[K, V](k: K): StateT[EitherInput, Table[K, V], Boolean] =
    isDefinedAt(k).map(r => !r)

  def missingReference[K: Gid](k: K): NonEmptyChain[InputError] =
    NonEmptyChain(InputError.fromMessage(s"missing reference ${Gid[K].show(k)}"))

  def lookup[K: Gid, V](k: K): StateT[EitherInput, Table[K, V], V] =
    StateT.inspectF(_.rows.get(k).toRight(missingReference(k)))

  def lookupOption[K, V](k: K): StateT[EitherInput, Table[K, V], Option[V]] =
    StateT.inspect(_.rows.get(k))

  def lookupAll[K: Gid, V](ks: List[K]): StateT[EitherInput, Table[K, V], List[V]] =
    StateT.inspectF { m =>
      ks.parTraverse(k => m.rows.get(k).toRight(missingReference(k)))
    }

  def findAll[K, V](f: ((K, V)) => Boolean): StateT[EitherInput, Map[K, V], List[V]] =
    StateT.inspect(_.filter(f).values.toList)

  def cycleNextUnused[K: Gid, V]: StateT[EitherInput, Table[K, V], K] = {
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

  def alreadyDefined[K: Gid, V, A](k: K): StateT[EitherInput, Table[K, V], A] =
    StateT.apply{ _ =>
      InputError.fromMessage(s"${Gid[K].show(k)} is already defined").leftNec[(Table[K, V], A)]
    }

  def getUnusedKey[K: Gid, V](suggestion: Option[K]): StateT[EitherInput, Table[K, V], K] =
    suggestion.fold(cycleNextUnused[K, V]) { k =>
      isEmptyAt[K, V](k).ifM(StateT.pure[EitherInput, Table[K, V], K](k), alreadyDefined[K, V, K](k))
    }

  def saveNew[K: Gid, V](k: K, v: V): StateT[EitherInput, Table[K, V], Unit] =
    isDefinedAt[K, V](k).ifM(alreadyDefined[K, V, Unit](k), update(k, v))

  def saveNewIfValid[K: Gid, V](v: ValidatedInput[V])(key: V => K): StateT[EitherInput, Table[K, V], Unit] =
    v.fold(
      nec => StateT.setF(nec.asLeft[Table[K, V]]),
      v   => saveNew(key(v), v)
    )

  def update[K, V](k: K, v: V): StateT[EitherInput, Table[K, V], Unit] =
    StateT.modify(Table.rows.modify(_ + (k -> v)))

  def delete[K, V](k: K): StateT[EitherInput, Table[K, V], Unit] =
    StateT.modify(Table.rows.modify(_ - k))

}

object TableState extends TableState

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.data.{NonEmptyChain, StateT}
import cats.kernel.BoundedEnumerable
import cats.syntax.parallel._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.monad._
import cats.syntax.option._
import cats.syntax.traverse._
import cats.syntax.validated._
import lucuma.core.util.Gid

trait TableState[K, V] {

  def isDefinedAt(k: K): StateT[EitherInput, Table[K, V], Boolean] =
    StateT.inspect(_.rows.isDefinedAt(k))

  def isEmptyAt(k: K): StateT[EitherInput, Table[K, V], Boolean] =
    isDefinedAt(k).map(r => !r)

  def missingReference(k: K)(implicit G: Gid[K]): NonEmptyChain[InputError] =
    NonEmptyChain(InputError.fromMessage(s"missing reference ${G.show(k)}"))

  def lookup(k: K)(implicit G: Gid[K]): StateT[EitherInput, Table[K, V], V] =
    StateT.inspectF(_.rows.get(k).toRight(missingReference(k)))

  def lookupValidated(k: K)(implicit G: Gid[K]): StateT[EitherInput, Table[K, V], ValidatedInput[V]] =
    lookupOption(k).map(_.toValid(missingReference(k)))

  def lookupOption(k: K): StateT[EitherInput, Table[K, V], Option[V]] =
    StateT.inspect(_.rows.get(k))

  def lookupAll(ks: List[K])(implicit G: Gid[K]): StateT[EitherInput, Table[K, V], List[V]] =
    StateT.inspectF { t =>
      ks.parTraverse(k => t.rows.get(k).toRight(missingReference(k)))
    }

  def lookupAllValidated(ks: List[K])(implicit G: Gid[K]): StateT[EitherInput, Table[K, V], ValidatedInput[List[V]]] =
    StateT.inspect { t =>
      ks.traverse(k => t.rows.get(k).toValid(missingReference(k)))
    }

  def findAll(f: ((K, V)) => Boolean): StateT[EitherInput, Table[K, V], List[V]] =
    StateT.inspect(_.rows.filter(f).values.toList)

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
    StateT.liftF(InputError.fromMessage(s"${G.show(k)} is already defined").leftNec[A])

  def getUnusedKey(suggestion: Option[K])(implicit G: Gid[K]): StateT[EitherInput, Table[K, V], K] =
    suggestion.fold(cycleNextUnused) { k =>
      isEmptyAt(k).ifM(StateT.pure[EitherInput, Table[K, V], K](k), alreadyDefined[K](k))
    }

  def getUnusedKeyValidated(suggestion: Option[K])(implicit G: Gid[K]): StateT[EitherInput, Table[K, V], ValidatedInput[K]] =
    suggestion.fold(cycleNextUnused.map(_.validNec[InputError])) { k =>
      isEmptyAt(k).ifM(
        StateT.pure[EitherInput, Table[K, V], ValidatedInput[K]](k.validNec[InputError]),
        StateT.pure[EitherInput, Table[K, V], ValidatedInput[K]](InputError.fromMessage(s"${G.show(k)} is already defined").invalidNec)
      )
    }

  def saveNew(k: K, v: V)(implicit G: Gid[K]): StateT[EitherInput, Table[K, V], Unit] =
    isDefinedAt(k).ifM(alreadyDefined[Unit](k), update(k, v))

  def saveNewIfValid(v: ValidatedInput[V])(key: V => K)(implicit G: Gid[K]): StateT[EitherInput, Table[K, V], Unit] =
    v.fold(
      nec => StateT.liftF(nec.asLeft[Unit]),
      v   => saveNew(key(v), v)
    )

  def update(k: K, v: V): StateT[EitherInput, Table[K, V], Unit] =
    StateT.modify(Table.rows.modify(_ + (k -> v)))

  def delete(k: K): StateT[EitherInput, Table[K, V], Unit] =
    StateT.modify(Table.rows.modify(_ - k))

}
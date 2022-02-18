// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.data.StateT
import lucuma.core.util.Gid
import monocle.Lens

final case class DatabaseState[K, V](
  lens: Lens[Database, Table[K, V]]
) {

  object table extends TableState[K, V]

  def transform[A](s: StateT[EitherInput, Table[K, V], A]): StateT[EitherInput, Database, A] =
    s.transformS(lens.get, (d, t) => lens.replace(t)(d))

  def isDefinedAt(k: K): StateT[EitherInput, Database, Boolean] =
    transform(table.isDefinedAt(k))

  def isEmptyAt(k: K): StateT[EitherInput, Database, Boolean] =
    transform(table.isEmptyAt(k))

  def lookup(k: K)(implicit G: Gid[K]): StateT[EitherInput, Database, V] =
    transform(table.lookup(k))

  def lookupValidated(k: K)(implicit G: Gid[K]): StateT[EitherInput, Database, ValidatedInput[V]] =
    transform(table.lookupValidated(k))

  def lookupOption(k: K): StateT[EitherInput, Database, Option[V]] =
    transform(table.lookupOption(k))

  def lookupAll(ks: List[K])(implicit G: Gid[K]): StateT[EitherInput, Database, List[V]] =
    transform(table.lookupAll(ks))

  def lookupAllValidated(ks: List[K])(implicit G: Gid[K]): StateT[EitherInput, Database, ValidatedInput[List[V]]] =
    transform(table.lookupAllValidated(ks))

  def findAll(f: ((K, V)) => Boolean): StateT[EitherInput, Database, List[V]] =
    transform(table.findAll(f))

  def cycleNextUnused(implicit G: Gid[K]): StateT[EitherInput, Database, K] =
    transform(table.cycleNextUnused)

  def getUnusedKey(suggestion: Option[K])(implicit G: Gid[K]): StateT[EitherInput, Database, K] =
    transform(table.getUnusedKey(suggestion))

  def getUnusedKeyValidated(suggestion: Option[K])(implicit G: Gid[K]): StateT[EitherInput, Database, ValidatedInput[K]] =
    transform(table.getUnusedKeyValidated(suggestion))

  def saveNew(k: K, v: V)(implicit G: Gid[K]): StateT[EitherInput, Database, Unit] =
    transform(table.saveNew(k, v))

  def saveNewIfValid(v: ValidatedInput[V])(key: V => K)(implicit G: Gid[K]): StateT[EitherInput, Database, Unit] =
    transform(table.saveNewIfValid(v)(key))

  def update(k: K, v: V): StateT[EitherInput, Database, Unit] =
    transform(table.update(k, v))

  def delete(k: K): StateT[EitherInput, Database, Unit] =
    transform(table.delete(k))

}

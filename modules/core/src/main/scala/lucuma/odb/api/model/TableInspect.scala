// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.data.{NonEmptyChain, StateT}
import cats.syntax.parallel._
import lucuma.core.util.Gid

trait TableInspect[K, V] {

  def isDefinedAt(k: K): StateT[EitherInput, Table[K, V], Boolean] =
    StateT.inspect(_.rows.isDefinedAt(k))

  def isEmptyAt(k: K): StateT[EitherInput, Table[K, V], Boolean] =
    isDefinedAt(k).map(r => !r)

  def missingReference(k: K)(implicit G: Gid[K]): NonEmptyChain[InputError] =
    NonEmptyChain(InputError.fromMessage(s"missing reference ${G.show(k)}"))

  def lookup(k: K)(implicit G: Gid[K]): StateT[EitherInput, Table[K, V], V] =
    StateT.inspectF(_.rows.get(k).toRight(missingReference(k)))

  def lookupOption(k: K): StateT[EitherInput, Table[K, V], Option[V]] =
    StateT.inspect(_.rows.get(k))

  def lookupAll(ks: List[K])(implicit G: Gid[K]): StateT[EitherInput, Table[K, V], List[V]] =
    StateT.inspectF { m =>
      ks.parTraverse(k => m.rows.get(k).toRight(missingReference(k)))
    }

  def findAll(f: ((K, V)) => Boolean): StateT[EitherInput, Map[K, V], List[V]] =
    StateT.inspect(_.filter(f).values.toList)

}

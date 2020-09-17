// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.syntax.lens._

import cats.implicits._
import cats.data.State
import monocle.Lens

/**
 * A trait for top-level objects.
 */
trait TopLevelModel[I, T] {

  def id(t: T): I

  def existence: Lens[T, Existence]

  def existenceEditor(i: I, s: Existence): Editor[I, T] =
    new Editor[I, T] {
      override def id: I =
        i

      override def editor: State[T, Unit] =
        (existence := Some(s)).void
    }
}

object TopLevelModel {

  def apply[I, T](implicit ev: TopLevelModel[I, T]): ev.type = ev

  def instance[I, T](getId: T => I, existenceLens: Lens[T, Existence]): TopLevelModel[I, T] =
    new TopLevelModel[I, T] {
      def id(t: T): I = getId(t)
      def existence: Lens[T, Existence] = existenceLens
    }

}

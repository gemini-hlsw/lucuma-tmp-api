// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.data.State

trait Editor[I, T] {

  def id: I

  def editor: State[T, Unit]

  def edit(t: T): T =
    editor.runS(t).value

}
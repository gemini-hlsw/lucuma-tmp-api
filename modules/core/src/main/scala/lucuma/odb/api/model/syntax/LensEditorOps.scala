// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.syntax

import cats.data.State
import monocle.Lens
import monocle.state.all._

final class LensEditorOps[S, A](self: Lens[S, A]) {

  def edit(a: Option[A]): State[S, A] =
    a.fold(self.st)(self.assign)

  def :=(a: Option[A]): State[S, A] =
    edit(a)

}

trait ToLensEditorOps {
  implicit def ToLensEditorOps[S, A](lens: Lens[S, A]): LensEditorOps[S, A] =
    new LensEditorOps[S, A](lens)
}

object lens extends ToLensEditorOps



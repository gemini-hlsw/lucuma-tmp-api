// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.syntax

import cats.data.State
import monocle.Optional
import monocle.state.all._

final class OptionalEditorOps[S, A](val self: Optional[S, A]) extends AnyVal {

  def edit(a: A): State[S, Option[A]] =
    self.assign(a)

  @inline def :=(a: A): State[S, Option[A]] =
    edit(a)

  def edit(a: Option[A]): State[S, Option[A]] =
    a.fold(self.st)(self.assign)

  @inline def :=(a: Option[A]): State[S, Option[A]] =
    edit(a)

}

trait ToOptionalEditorOps {
  implicit def ToOptionalEditorOps[S, A](opt: Optional[S, A]): OptionalEditorOps[S, A] =
    new OptionalEditorOps[S, A](opt)
}

object optional extends ToOptionalEditorOps



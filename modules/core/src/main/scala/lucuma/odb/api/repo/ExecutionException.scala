// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.core.util.Gid
import cats.MonadError

/**
 * An exception due to an inconsistent state in the repository.  In other words,
 * a programming error rather than a user-input error.
 */
final case class ExecutionException(message: String) extends Exception(message)

object ExecutionException {

  def missingReference[F[_], I, A](id: I)(implicit M: MonadError[F, Throwable], G: Gid[I]): F[A] =
    M.raiseError[A](ExecutionException(s"Missing reference ${Gid[I].show(id)}"))
}

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.{Applicative, ApplicativeError}
import cats.data.State

trait Editor[I, T] {

  def id: I

  def editor: ValidatedInput[State[T, Unit]]

  def edit(t: T): ValidatedInput[T] =
    editor.map(_.runS(t).value)

  def validateOrError[F[_]: Applicative](implicit M: ApplicativeError[F, Throwable]): F[State[T, Unit]] =
    editor.fold(
      err => M.raiseError[State[T, Unit]](InputError.Exception(err)),
      st  => implicitly[Applicative[F]].pure(st)
    )

}
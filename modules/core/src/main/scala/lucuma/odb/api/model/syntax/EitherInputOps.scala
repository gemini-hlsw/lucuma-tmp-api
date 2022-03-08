// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.syntax

import cats.ApplicativeError
import cats.syntax.either._
import lucuma.odb.api.model.{EitherInput, InputError}

final class EitherInputOps[A](self: EitherInput[A]) {

  def liftTo[F[_]](implicit F: ApplicativeError[F, Throwable]): F[A] =
    self.leftMap(InputError.Exception.apply).liftTo[F]

}

trait ToEitherInputOps {
  implicit def toEitherInputOps[A](e: EitherInput[A]): EitherInputOps[A] =
    new EitherInputOps[A](e)
}

object eitherinput extends ToEitherInputOps

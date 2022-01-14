// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.syntax

import cats.data.StateT
import lucuma.odb.api.model.EitherInput
import monocle.Optional

final class OptionalOps[S, A](val self: Optional[S, A]) extends AnyVal {

  def toState: StateT[EitherInput, S, Option[A]] =
    StateT(s => Right((s, self.getOption(s))))

  def st: StateT[EitherInput, S, Option[A]] =
    toState

  def extract: StateT[EitherInput, S, Option[A]] =
    toState

  def edit(a: A): StateT[EitherInput, S, Option[A]] =
    assign(a)

  @inline def :=(a: A): StateT[EitherInput, S, Option[A]] =
    edit(a)

  def edit(a: Option[A]): StateT[EitherInput, S, Option[A]] =
    a.fold(st)(assign)

  @inline def :=(a: Option[A]): StateT[EitherInput, S, Option[A]] =
    edit(a)

  def mod(f: A => A): StateT[EitherInput, S, Option[A]] =
    StateT { s0 =>
      val s1 = self.modify(f)(s0)
      Right((s1, self.getOption(s1)))
    }

  def modo(f: A => A): StateT[EitherInput, S, Option[A]] =
    StateT(s => Right((self.modify(f)(s), self.getOption(s))))

  def mod_(f: A => A): StateT[EitherInput, S, Unit] =
    StateT(s => Right((self.modify(f)(s), ())))

  def assign(a: A): StateT[EitherInput, S, Option[A]] =
    mod(_ => a)

  def assigno(a: A): StateT[EitherInput, S, Option[A]] =
    modo(_ => a)

  def assign_(a: A): StateT[EitherInput, S, Unit] =
    mod_(_ => a)
}

trait ToOptionalOps {
  implicit def ToOptionalOps[S, A](opt: Optional[S, A]): OptionalOps[S, A] =
    new OptionalOps[S, A](opt)
}

object optional extends ToOptionalOps

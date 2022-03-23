// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.syntax

import cats.Applicative
import cats.data.StateT
import lucuma.odb.api.model.EitherInput
import monocle.Optional

final class OptionalOps[S, A](val self: Optional[S, A]) extends AnyVal {

  def toState[F[_]: Applicative]: StateT[F, S, Option[A]] =
    StateT(s => Applicative[F].pure((s, self.getOption(s))))

  def st[F[_]: Applicative]: StateT[F, S, Option[A]] =
    toState

  def extract[F[_]: Applicative]: StateT[F, S, Option[A]] =
    toState

  def edit[F[_]: Applicative](a: A): StateT[F, S, Option[A]] =
    assign(a)

  @inline def :=(a: A): StateT[EitherInput, S, Option[A]] =
    edit[EitherInput](a)

  def edit[F[_]: Applicative](a: Option[A]): StateT[F, S, Option[A]] =
    a.fold(st[F])(assign[F])

  @inline def :=(a: Option[A]): StateT[EitherInput, S, Option[A]] =
    edit(a)

  def mod[F[_]: Applicative](f: A => A): StateT[F, S, Option[A]] =
    StateT { s0 =>
      val s1 = self.modify(f)(s0)
      Applicative[F].pure((s1, self.getOption(s1)))
    }

  def modo[F[_]: Applicative](f: A => A): StateT[F, S, Option[A]] =
    StateT(s => Applicative[F].pure((self.modify(f)(s), self.getOption(s))))

  def mod_[F[_]: Applicative](f: A => A): StateT[F, S, Unit] =
    StateT(s => Applicative[F].pure((self.modify(f)(s), ())))

  def assign[F[_]: Applicative](a: A): StateT[F, S, Option[A]] =
    mod(_ => a)

  def assigno[F[_]: Applicative](a: A): StateT[F, S, Option[A]] =
    modo(_ => a)

  def assign_[F[_]: Applicative](a: A): StateT[F, S, Unit] =
    mod_(_ => a)
}

trait ToOptionalOps {
  implicit def ToOptionalOps[S, A](opt: Optional[S, A]): OptionalOps[S, A] =
    new OptionalOps[S, A](opt)
}

object optional extends ToOptionalOps

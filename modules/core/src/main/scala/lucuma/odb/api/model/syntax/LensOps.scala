// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.syntax

import cats.data.StateT
import lucuma.odb.api.model.EitherInput
import monocle.Lens

final class LensOps[S, A](val self: Lens[S, A]) extends AnyVal {

  def transform(st: StateT[EitherInput, A, Unit]): StateT[EitherInput, S, Unit] =
    st.transformS[S](self.get, (s, a) => self.replace(a)(s))

  def toState: StateT[EitherInput, S, A] =
    StateT(s => Right((s, self.get(s))))

  def st: StateT[EitherInput, S, A] =
    toState

  def extract: StateT[EitherInput, S, A] =
    toState

  def edit(a: A): StateT[EitherInput, S, A] =
    assign(a)

  @inline def :=(a: A): StateT[EitherInput, S, A] =
    edit(a)

  def edit(a: Option[A]): StateT[EitherInput, S, A] =
    a.fold(st)(assign)

  @inline def :=(a: Option[A]): StateT[EitherInput, S, A] =
    edit(a)

  def mod(f: A => A): StateT[EitherInput, S, A] =
    StateT { s0 =>
      val s1 = self.modify(f)(s0)
      Right((s1, self.get(s1)))
    }

  def modo(f: A => A): StateT[EitherInput, S, A] =
    StateT(s => Right((self.modify(f)(s), self.get(s))))

  def mod_(f: A => A): StateT[EitherInput, S, Unit] =
    StateT(s => Right((self.modify(f)(s), ())))

  def assign(a: A): StateT[EitherInput, S, A] =
    mod(_ => a)

  def assigno(a: A): StateT[EitherInput, S, A] =
    modo(_ => a)

  def assign_(a: A): StateT[EitherInput, S, Unit] =
    mod_(_ => a)
}

trait ToLensOps {
  implicit def ToLensOps[S, A](lens: Lens[S, A]): LensOps[S, A] =
    new LensOps[S, A](lens)
}

object lens extends ToLensOps

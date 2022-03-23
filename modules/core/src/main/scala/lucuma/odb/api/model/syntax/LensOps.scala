// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.syntax

import cats.Applicative
import cats.data.StateT
import clue.data.{Assign, Ignore, Input, Unassign}
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.option._
import lucuma.odb.api.model.{EditorInput, EitherInput, InputError}
import monocle.Lens

final class LensOps[S, A](val self: Lens[S, A]) extends AnyVal {

  /**
   * Uses the lens to create an editor of a nullable field of type `A` in `S`.
   * When assigning the value, if there is an existing value of type `A` then
   * it may be edited (which implies only specifying changes).  If there is no
   * existing `A` then it must be created (which implies specifying all required
   * values).
   */
  def :?[B](input: Input[EditorInput[B]])(implicit ev: Option[B] =:= A): StateT[EitherInput, S, Unit] =
    input match {

      // Do nothing.
      case Ignore    =>
        StateT.empty[EitherInput, S, Unit]

      // Unset the A in S.
      case Unassign  =>
        edit[EitherInput](Option.empty[B]).void

      // Create a new instance of A for the state S or else edit the existing
      // A instance in the state S.  Arguments to the input are validated
      // appropriately either way.

      case Assign(n) =>
        StateT.modifyF[EitherInput, S] { s =>
          self
            .modifyA[EitherInput] { a =>
                ev.flip(a)
                  .fold(n.create.toEither)(n.edit.runS)
                  .map(b => ev(b.some))
            }(s)
        }

    }

  /**
   * Uses the lens to create an editor of a required, non-nullable field of type
   * `A` in `S`.  Since there will always be a value of type `A`, it can always
   * be edited and need not be created with all required inputs.
   */
  def :!(input: Input[EditorInput[A]]): StateT[EitherInput, S, Unit] =
    input match {

      // Do nothing
      case Ignore    =>
        StateT.empty[EitherInput, S, Unit]

      // Cannot unset.  This is caught at a higher level in reality.
      case Unassign  =>
        StateT.setF(InputError.fromMessage("Cannot un-assign value").leftNec)

      case Assign(n) =>
        StateT.modifyF[EitherInput, S](self.modifyF(n.edit.runS))

    }

  def transform[F[_]: Applicative](st: StateT[F, A, Unit]): StateT[F, S, Unit] =
    st.transformS[S](self.get, (s, a) => self.replace(a)(s))

  def :<(st: Option[StateT[EitherInput, A, Unit]]): StateT[EitherInput, S, Unit] =
    st.fold(StateT.empty[EitherInput, S, Unit])(transform[EitherInput])

  def toState[F[_]: Applicative]: StateT[F, S, A] =
    StateT(s => Applicative[F].pure((s, self.get(s))))

  def st[F[_]: Applicative]: StateT[F, S, A] =
    toState

  def extract[F[_]: Applicative]: StateT[F, S, A] =
    toState

  def edit[F[_]: Applicative](a: A): StateT[F, S, A] =
    assign(a)

  @inline def :=(a: A): StateT[EitherInput, S, A] =
    edit[EitherInput](a)

  def edit[F[_]: Applicative](a: Option[A]): StateT[F, S, A] =
    a.fold(st)(assign)

  @inline def :=(a: Option[A]): StateT[EitherInput, S, A] =
    edit[EitherInput](a)

  def mod[F[_]: Applicative](f: A => A): StateT[F, S, A] =
    StateT { s0 =>
      val s1 = self.modify(f)(s0)
      Applicative[F].pure((s1, self.get(s1)))
    }

  def modo[F[_]: Applicative](f: A => A): StateT[F, S, A] =
    StateT(s => Applicative[F].pure((self.modify(f)(s), self.get(s))))

  def mod_[F[_]: Applicative](f: A => A): StateT[F, S, Unit] =
    StateT(s => Applicative[F].pure((self.modify(f)(s), ())))

  def assign[F[_]: Applicative](a: A): StateT[F, S, A] =
    mod(_ => a)

  def assigno[F[_]: Applicative](a: A): StateT[F, S, A] =
    modo(_ => a)

  def assign_[F[_]: Applicative](a: A): StateT[F, S, Unit] =
    mod_(_ => a)
}

trait ToLensOps {
  implicit def ToLensOps[S, A](lens: Lens[S, A]): LensOps[S, A] =
    new LensOps[S, A](lens)
}

object lens extends ToLensOps

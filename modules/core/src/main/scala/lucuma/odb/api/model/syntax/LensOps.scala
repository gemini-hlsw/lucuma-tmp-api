// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.syntax

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
        edit(Option.empty[B]).void

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

  def transform(st: StateT[EitherInput, A, Unit]): StateT[EitherInput, S, Unit] =
    st.transformS[S](self.get, (s, a) => self.replace(a)(s))

  def :<(st: Option[StateT[EitherInput, A, Unit]]): StateT[EitherInput, S, Unit] =
    st.fold(StateT.empty[EitherInput, S, Unit])(transform)

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

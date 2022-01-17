// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.syntax

import cats.syntax.either._
import cats.data.StateT
import lucuma.odb.api.model.{EitherInput, InputError}
import monocle.Prism

final class PrismOps[S, A](val self: Prism[S, A]) extends AnyVal {

  /**
   * Takes an editor for A to an editor for S, where a Prism[S, A] exists.
   * Ignores the case where `getOption` returns None, leaving the state
   * unmodified.
   */
  def transformOrIgnore(st: StateT[EitherInput, A, Unit]): StateT[EitherInput, S, Unit] =
    StateT[EitherInput, S, Unit] { s =>
      self.getOption(s).fold((s, ()).rightNec[InputError]) { a =>
        st.runS(a).map(aʹ => (self.replace(aʹ)(s), ()))
      }
    }

  /**
   * Takes an editor for A to an editor for S, where a Prism[S, A] exists.
   * Produces an error for the case where `getOption` returns None.
   */
  def transformOrFail(
    st:  StateT[EitherInput, A, Unit],
    msg: => String
  ): StateT[EitherInput, S, Unit] =
    StateT[EitherInput, S, Unit] { s =>
      self.getOption(s).fold(InputError.fromMessage(msg).leftNec[(S, Unit)]) { a =>
        st.runS(a).map(aʹ => (self.replace(aʹ)(s), ()))
      }
    }

}

trait ToPrismOps {

  implicit def ToPrismOps[S, A](prism: Prism[S, A]): PrismOps[S, A] =
    new PrismOps[S, A](prism)

}

object prism extends ToPrismOps
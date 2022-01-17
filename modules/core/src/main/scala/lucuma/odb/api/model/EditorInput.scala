// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.data.StateT
import clue.data.{Assign, Ignore, Input, Unassign}
import cats.syntax.functor._
import cats.syntax.option._
import lucuma.odb.api.model.syntax.optional._
import monocle.Optional

/**
 * Describes how to validate arguments and create a new A or else validate
 * arguments and edit an existing A.
 *
 * @tparam A type of the editing target
 */
trait EditorInput[A] {

  def create: ValidatedInput[A]

  def edit: StateT[EitherInput, A, Unit]

}

object EditorInput {

  /**
   * Using the provided optic, applies the update described by the given `Input`
   * in the resulting state computation.
   */
  def nullable[S, A, N <: EditorInput[A]](
    optional: Optional[S, Option[A]],
    input:    Input[N]
  ): StateT[EitherInput, S, Unit] =

    input match {

      // Do nothing.
      case Ignore    =>
        StateT.empty[EitherInput, S, Unit]

      // Unset the A in S.
      case Unassign  =>
        (optional := Option.empty[A].some).void

      // Create a new instance of A for the state S or else edit the existing
      // A instance in the state S.  Arguments to the input are validated
      // appropriately either way.
      case Assign(n) =>
        StateT.modifyF[EitherInput, S] { s =>
          optional
            .modifyA[EitherInput](_.fold(n.create.toEither)(n.edit.runS).map(_.some))(s)
        }
    }

}

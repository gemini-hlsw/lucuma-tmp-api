// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.syntax

import clue.data.Input
import cats.syntax.traverse._
import cats.syntax.validated._
import lucuma.odb.api.model.{InputError, ValidatedInput}

final class InputOps[A](val self: Input[A]) extends AnyVal {

  def toOptionOption: Option[Option[A]] =
    InputOps.toOptionOption(self)

  def validateNullable[B](f: A => ValidatedInput[B]): ValidatedInput[Option[Option[B]]] =
    self.traverse(f).map(InputOps.toOptionOption)

  def validateNotNullable[B](name: => String)(f: A => ValidatedInput[B]): ValidatedInput[Option[B]] =
    self.fold(
      Option.empty[B].validNec[InputError],
      InputError.fromMessage(s"'$name' cannot be unassigned").invalidNec[Option[B]],
      a => f(a).map(Some(_))
    )

  def validateIsNotNull(name: => String): ValidatedInput[Option[A]] =
    validateNotNullable(name)(_.validNec[InputError])

}

object InputOps {

  private def toOptionOption[A](input: Input[A]): Option[Option[A]] =
    input.fold(
      Option.empty[Option[A]],
      Some(Option.empty[A]),
      a => Some(Some(a))
    )

}

trait ToInputOps {
  implicit def ToInputOps[A](input: Input[A]): InputOps[A] =
    new InputOps[A](input)
}

object input extends ToInputOps

// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.syntax

import clue.data.Input //{Input, Assign, Ignore, Unassign}
import cats.syntax.all._
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

final class InputCompanionOps(val self: Input.type) extends AnyVal {


  // Two fields A and B but one or the other, or neither, may be assigned.
  // Assuming this condition is met, returns a function that accepts the current
  // value of the Option[Either[A, B]] field and returns the new value.
  def validateNullableEither[A, B](nameA: String, nameB: String, a: Input[A], b: Input[B]): ValidatedInput[Option[Either[A, B]] => Option[Either[A, B]]] = {
    type OE  = Option[Either[A, B]]
    val none: OE = Option.empty

    // Hmm, cannot match Input directly so i'll turn them into Option[Option[A]]
    //  [error] /Users/swalker/dev/lucuma-tmp-api/modules/core/src/main/scala/lucuma/odb/api/model/syntax/InputOps.scala:55:12: error during expansion of this match (this is a scalac bug).

    (new InputOps[A](a).toOptionOption, new InputOps[B](b).toOptionOption) match {
      // assign both
      case (Some(Some(_)),  Some(Some(_)))  => InputError.fromMessage(s"Cannot assign both $nameA and $nameB").invalidNec[OE => OE]

      // assign one or the other
      case (Some(Some(a0)), _)              => ((_: OE)  => a0.asLeft[B].some).validNec[InputError]
      case (_,              Some(Some(b0))) => ((_: OE)  => b0.asRight[A].some).validNec[InputError]

      // unassign one or the other or both.
      case (None,           Some(None))     => ((oe: OE) => oe.flatMap(_.fold(_ => oe, _ => none))).validNec[InputError]
      case (Some(None),     None)           => ((oe: OE) => oe.flatMap(_.fold(_ => none, _ => oe))).validNec[InputError]
      case (Some(None),     Some(None))     => ((_: OE)  => none).validNec[InputError]

      // do nothing
      case (None,           None)           => ((oe: OE) => oe).validNec[InputError]

    }
    /*
    (a, b) match {
      case (Assign(_),  Assign(_))  => InputError.fromMessage(s"Cannot assign both $nameA and $nameB").invalidNec[OE => OE]

      case (Assign(a0), _)          => ((_: OE)  => a0.asLeft[B].some).validNec[InputError]
      case (_,          Assign(b0)) => ((_: OE)  => b0.asRight[A].some).validNec[InputError]

      case (Ignore,     Unassign)   => ((oe: OE) => oe.flatMap(_.fold(_ => oe, _ => none))).validNec[InputError]
      case (Unassign,   Ignore)     => ((oe: OE) => oe.flatMap(_.fold(_ => none, _ => oe))).validNec[InputError]

      case (Ignore,     Ignore)     => ((oe: OE) => oe).validNec[InputError]
      case (Unassign,   Unassign)   => ((_: OE)  => none).validNec[InputError]
    }
     */
  }

}

trait ToInputCompanionOps {
  implicit def ToInputCompanionOps(c: Input.type): InputCompanionOps =
    new InputCompanionOps(c)
}

object input extends ToInputOps with ToInputCompanionOps

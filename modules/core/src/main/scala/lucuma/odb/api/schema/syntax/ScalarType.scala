// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema.syntax

import cats.implicits._
import lucuma.odb.api.model.format.ScalarFormat
import sangria.ast.StringValue
import sangria.schema.{InputType, OptionInputType, ScalarType}
import sangria.validation.{ValueCoercionViolation, Violation}

final case class FormatViolation(msg: String) extends ValueCoercionViolation(msg)

final class ScalarTypeCompanionOps(val self: ScalarType.type) extends AnyVal {

  private def errorMessage[A](f: ScalarFormat[A]): String =
    s"Expected format '${f.hint}'"

  private def fail[A](f: ScalarFormat[A]): Either[Violation, A] =
    FormatViolation(errorMessage(f)).asLeft[A]

  def tryParse[A](f: ScalarFormat[A], s: String): Either[Violation, A] =
    f.format.getOption(s).toRight(FormatViolation(errorMessage(f)))

  /**
   * Defines a `ScalarType` from a name, description, and a `ScalarFormat`.
   * @param name         name of the new scalar type
   * @param description  description of the scalar type
   * @param scalarFormat `Format` and formatting hint
   */
  def fromScalarFormat[A](
    name:         String,
    description:  String,
    scalarFormat: ScalarFormat[A]
  ): ScalarType[A] =

    ScalarType[A](
      name            = name,
      description     = Some(s"$description in format '${scalarFormat.hint}'"),
      coerceUserInput = {
        case s: String => tryParse(scalarFormat, s)
        case _         => fail(scalarFormat)
      },
      coerceOutput    = (t, _) => scalarFormat.format.reverseGet(t),
      coerceInput     = {
        case StringValue(s, _, _, _, _) => tryParse(scalarFormat, s)
        case _                          => fail(scalarFormat)
      }
    )

}

trait ToScalarTypeCompanionOps {
  implicit def ToScalarTypeCompanionOps(c: ScalarType.type): ScalarTypeCompanionOps =
    new ScalarTypeCompanionOps(c)
}

final class ScalarTypeOps[A](val self: ScalarType[A]) extends AnyVal {

  def optional: InputType[Option[A]] =
    OptionInputType(self)

}

trait ToScalarTypeOps {
  implicit def ToScalarTypeOps[A](scalarType: ScalarType[A]): ScalarTypeOps[A] =
    new ScalarTypeOps[A](scalarType)
}

object scalar extends ToScalarTypeCompanionOps with ToScalarTypeOps

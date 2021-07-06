// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.util.Gid
import sangria.ast
import sangria.schema.ScalarType
import sangria.validation.ValueCoercionViolation

// This is barely different than the FormattedStringSchema and we could probably
// just use that.

/**
 * Object ID schema.
 */
object ObjectIdSchema {

  private def formatString[A: Gid]: String =
    s"${Gid[A].tag}-([1-9a-f][0-9a-f]*)"

  final case class IdViolation[A: Gid]() extends ValueCoercionViolation(s"Expected an ID of type ${formatString[A]}")

  def idType[A: Gid](name: String): ScalarType[A] =
    ScalarType[A](
      name            = name,
      description     = Some(s"$name id formatted as `${formatString[A]}`"),
      coerceUserInput = {
        case s: String => Gid[A].fromString.getOption(s).toRight(IdViolation[A]())
        case _         => Left(IdViolation[A]())
      },
      coerceOutput    = (a, _) => Gid[A].show(a),
      coerceInput     = {
        case ast.StringValue(s, _, _, _, _) => Gid[A].fromString.getOption(s).toRight(IdViolation[A]())
        case _                              => Left(IdViolation[A]())
      }
    )

}

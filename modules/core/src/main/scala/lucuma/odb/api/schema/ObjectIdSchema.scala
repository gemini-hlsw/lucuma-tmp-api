// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.util.Gid
import lucuma.odb.api.model.Uid
import monocle.Prism
import sangria.ast
import sangria.schema.ScalarType
import sangria.validation.ValueCoercionViolation

/**
 * Object ID schema.
 */
object ObjectIdSchema {

  final case class IdViolation(pattern: String) extends ValueCoercionViolation(s"Expected an ID of type $pattern")

  private def idType[A](
    name:    String,
    pattern: String,
    prism:   Prism[String, A]
  ): ScalarType[A] =
    ScalarType[A](
      name            = name,
      description     = Some(s"$name id formatted as `$pattern`"),
      coerceUserInput = {
        case s: String => prism.getOption(s).toRight(IdViolation(pattern))
        case _         => Left(IdViolation(pattern))
      },
      coerceOutput    = (a, _) => prism.reverseGet(a),
      coerceInput     = {
        case ast.StringValue(s, _, _, _, _) => prism.getOption(s).toRight(IdViolation(pattern))
        case _                              => Left(IdViolation(pattern))
      }
    )

  private def stripGrouping(pattern: String): String =
    pattern
      .replace("(", "")
      .replace(")", "")

  def gidType[A: Gid](name: String): ScalarType[A] =
    idType[A](
      name,
      stripGrouping(Gid[A].regexPattern),
      Gid[A].fromString
    )

  def uidType[A: Uid](name: String): ScalarType[A] =
    idType[A](
      name,
      stripGrouping(Uid[A].regexPattern),
      Uid[A].fromString
    )

  // A ScalarAlias would work, but doesn't allow changing the description to
    // document the format
//    ScalarAlias[A, String](
//      StringType,
//      Uid[A].show,
//      s => Uid[A].fromString.getOption(s).toRight(UidViolation[A]())
//    ).rename(name)
}

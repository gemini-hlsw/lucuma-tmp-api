// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema.syntax

import lucuma.core.syntax.display._
import lucuma.core.syntax.enumerated._
import lucuma.core.syntax.string._

import lucuma.core.util.{Display, Enumerated}
import sangria.schema.{EnumType, EnumValue}

final class EnumTypeCompanionOps(val self: EnumType.type) {

  private def enumType[A: Enumerated](
    name:        String,
    description: String,
    valueDescription: A => String
  ): EnumType[A] =

    EnumType(
      name        = name,
      description = Some(description),
      values      = Enumerated[A].all.map { e =>
        EnumValue(
          name        = e.tag.toScreamingSnakeCase,
          description = Some(valueDescription(e)),
          value       = e
        )
      }
    )

  def fromEnumeratedWithDisplay[A: Enumerated: Display](
    name:        String,
    description: String
  ): EnumType[A] =

    enumType[A](name, description, a => s"$name ${a.longName}")

  def fromEnumerated[A: Enumerated](
    name:        String,
    description: String
  ): EnumType[A] =

    enumType[A](name, description, a => s"$name ${a.tag}")

}

trait ToEnumTypeCompanionOps {
  implicit def toEnumTypeCompanionOps(c: EnumType.type): EnumTypeCompanionOps =
    new EnumTypeCompanionOps(c)
}

object enum extends ToEnumTypeCompanionOps
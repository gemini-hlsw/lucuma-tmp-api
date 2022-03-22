// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema.syntax

import lucuma.core.syntax.display._
import lucuma.core.syntax.enumerated._
import lucuma.core.syntax.string._
import lucuma.core.util.{Display, Enumerated}
import sangria.schema.{EnumType, EnumValue, InputType, ListInputType, OptionInputType}

final class EnumTypeCompanionOps(val self: EnumType.type) {

  def fromEnumeratedWithDisplay[A: Enumerated: Display](
    name:        String,
    description: String
  ): EnumType[A] =

    fromEnumeratedMapping[A](name, description, _.tag, a => s"$name ${a.longName}")

  def fromEnumerated[A: Enumerated](
    name:        String,
    description: String
  ): EnumType[A] =

    fromEnumeratedMapping[A](name, description, _.tag, a => s"$name ${a.tag}")

  def fromEnumeratedMapping[A: Enumerated](
    name:        String,
    description: String,
    valueName:        A => String,
    valueDescription: A => String
  ): EnumType[A] =

    EnumType(
      name        = name,
      description = Some(description),
      values      = Enumerated[A].all.map { a =>
        EnumValue(
          name        = valueName(a).toScreamingSnakeCase,
          description = Some(valueDescription(a)),
          value       = a
        )
      }
    )


}

trait ToEnumTypeCompanionOps {
  implicit def toEnumTypeCompanionOps(c: EnumType.type): EnumTypeCompanionOps =
    new EnumTypeCompanionOps(c)
}

final class EnumTypeOps[A](val self: EnumType[A]) extends AnyVal {

  def optional: InputType[Option[A]] =
    OptionInputType(self)

  def list: InputType[Seq[A]] =
    ListInputType(self)

}

trait ToEnumTypeOps {
  implicit def ToEnumTypeOps[A](enumType: EnumType[A]): EnumTypeOps[A] =
    new EnumTypeOps[A](enumType)
}

object `enum` extends ToEnumTypeCompanionOps with ToEnumTypeOps
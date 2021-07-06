// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema.syntax

import sangria.schema._

final class InputTypeOps[A](self: InputType[A]) {

  def nullableField(name: String): InputField[Option[A]] =
    InputField(
      name,
      OptionInputType(self),
      s"The $name field may be unset by assigning a null value, or ignored by skipping it altogether"
    )

  def notNullableField(name: String): InputField[Option[A]] =
    InputField(
      name,
      OptionInputType(self),
      s"The $name field must be either specified or skipped altogether.  It cannot be unset with a null value."
    )

}

trait ToInputTypeOps {
  implicit def toInputTypeOps[A](self: InputType[A]): InputTypeOps[A] =
    new InputTypeOps[A](self)
}

object inputtype extends ToInputTypeOps

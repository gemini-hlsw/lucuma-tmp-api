// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema.syntax

import sangria.schema._

final class InputTypeOps[A](self: InputType[A]) {

  def optionField(name: String): InputField[Option[A]] =
    InputField(name, OptionInputType(self))

  def optionField(name: String, message: String): InputField[Option[A]] =
    InputField(name, OptionInputType(self), message)

  def nullableField(name: String): InputField[Option[A]] =
    optionField(
      name,
      s"The $name field may be unset by assigning a null value, or ignored by skipping it altogether"
    )

  def notNullableField(name: String): InputField[Option[A]] =
    optionField(
      name,
      s"The $name field must be either specified or skipped altogether.  It cannot be unset with a null value."
    )

  def createRequiredEditOptional(name: String, item: String): InputField[Option[A]] =
    optionField(
      name,
      s"The $name field is required when creating a new instance of $item, but optional when editing"
    )

}

trait ToInputTypeOps {
  implicit def toInputTypeOps[A](self: InputType[A]): InputTypeOps[A] =
    new InputTypeOps[A](self)
}

object inputtype extends ToInputTypeOps

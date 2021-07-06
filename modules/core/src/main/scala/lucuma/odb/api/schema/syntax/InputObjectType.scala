// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema.syntax

import sangria.marshalling.FromInput
import sangria.schema._

final class InputObjectTypeOps[A: FromInput](val self: InputObjectType[A]) {

  def argument(
    name:        String,
    description: String
  ): Argument[A] =
    Argument(
      name         = name,
      argumentType = self,
      description  = description
    )

  def optional: InputType[Option[A]] =
    OptionInputType(self)

}

trait ToInputObjectTypeOps {
  implicit def toInputObjectTypeOps[A: FromInput](self: InputObjectType[A]): InputObjectTypeOps[A] =
    new InputObjectTypeOps[A](self)
}

object inputobjecttype extends ToInputObjectTypeOps

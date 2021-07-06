// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema.syntax

import sangria.marshalling.ToInput

final class ToInputOps[A, R](val self: ToInput[A, R]) {

  // N.B. I doubt this is a contravariant functor so we'll just add it as syntax for now.
  def contramap[B](f: B => A): ToInput[B, R] =
    (value: B) => self.toInput(f(value))

}

trait ToToInputOps {
  implicit def toToInputOps[A, R](self: ToInput[A, R]): ToInputOps[A, R] =
    new ToInputOps(self)
}

final class ToInputCompanionOps(val self: ToInput.type) extends AnyVal {

  def apply[A, R](implicit ev: ToInput[A, R]): ev.type = ev

}

trait ToToInputCompanionOps {
  implicit def toToInputCompanionOps(self: ToInput.type): ToInputCompanionOps =
    new ToInputCompanionOps(self)
}

object toinput
  extends ToToInputOps
     with ToToInputCompanionOps

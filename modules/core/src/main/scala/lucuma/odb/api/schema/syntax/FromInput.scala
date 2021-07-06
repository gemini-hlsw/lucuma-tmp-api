// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema.syntax

import sangria.marshalling.FromInput

final class FromInputOps[A](val self: FromInput[A]) {

  // N.B. I doubt this is a functor so we'll just add it as syntax for now.
  def map[B](f: A => B): FromInput[B] =
    new FromInput[B] {
      val marshaller: self.marshaller.type = self.marshaller
      def fromResult(node: marshaller.Node): B = f(self.fromResult(node))
    }

}

trait ToFromInputOps {
  implicit def toFromInputOps[A](self: FromInput[A]): FromInputOps[A] =
    new FromInputOps(self)
}

final class FromInputCompanionOps(val self: FromInput.type) extends AnyVal {

  def apply[A](implicit ev: FromInput[A]): ev.type = ev

}

trait ToFromInputCompanionOps {
  implicit def toFromInputCompanionOps(self: FromInput.type): FromInputCompanionOps =
    new FromInputCompanionOps(self)
}

object frominput
  extends ToFromInputOps
     with ToFromInputCompanionOps

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.syntax

import cats.syntax.traverse._
import io.circe.{Decoder, HCursor}

final class HCursorOps(self: HCursor) {

  def editor[A: Decoder](field: String): Decoder.Result[Option[A]] =
    self.get[Option[A]](field)

  def optionEditor[A: Decoder](field: String): Decoder.Result[Option[Option[A]]] =
    self.downField(field).success.traverse { hc =>
      if (hc.keys.exists(_.nonEmpty)) hc.as[Option[A]]
      else Right(Option.empty[A])
    }

}

trait ToHCursorOps {
  implicit def toHCursorOps(c: HCursor): HCursorOps =
    new HCursorOps((c))
}

object hcursor extends ToHCursorOps

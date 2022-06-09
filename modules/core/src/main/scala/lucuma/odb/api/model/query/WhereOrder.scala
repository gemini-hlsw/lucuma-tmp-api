// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.query

import cats.Order
import cats.syntax.order._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

final case class WhereOrder[A: Order](
  eq:  Option[A],
  neq: Option[A],
  gt:  Option[A],
  lt:  Option[A],
  gte: Option[A],
  lte: Option[A],
  in:  Option[List[A]],
  nin: Option[List[A]]
) extends WherePredicate[A] {

  def matches(a: A): Boolean =
    eq.forall(a === _)         &&
      neq.forall(a =!= _)      &&
      gt.forall(a > _)         &&
      lt.forall(a < _)         &&
      gte.forall(a >= _)       &&
      lte.forall(a <= _)       &&
      in.forall(_.contains(a)) &&
      nin.forall(!_.contains(a))

}

object WhereOrder {

  implicit def DecoderOrderFilter[A: Decoder: Order]: Decoder[WhereOrder[A]] =
    deriveDecoder[WhereOrder[A]]

}

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.query

import cats.Order
import cats.syntax.order._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

final case class WhereOrderInput[A: Order](
  EQ:  Option[A]       = None,
  NEQ: Option[A]       = None,
  GT:  Option[A]       = None,
  LT:  Option[A]       = None,
  GTE: Option[A]       = None,
  LTE: Option[A]       = None,
  IN:  Option[List[A]] = None,
  NIN: Option[List[A]] = None
) extends WherePredicate[A] {

  override def matches(a: A): Boolean =
    EQ.forall(a === _)         &&
      NEQ.forall(a =!= _)      &&
      GT.forall(a > _)         &&
      LT.forall(a < _)         &&
      GTE.forall(a >= _)       &&
      LTE.forall(a <= _)       &&
      IN.forall(_.contains(a)) &&
      NIN.forall(!_.contains(a))

}

object WhereOrderInput {

  implicit def DecoderWhereOrderInput[A: Decoder: Order]: Decoder[WhereOrderInput[A]] =
    deriveDecoder[WhereOrderInput[A]]

}

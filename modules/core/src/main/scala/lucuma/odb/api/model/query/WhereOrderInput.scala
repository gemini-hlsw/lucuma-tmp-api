// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.query

import cats.Order
import cats.syntax.option._
import cats.syntax.order._
import io.circe.Decoder
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredDecoder

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

  def EQ[A: Order](a: A): WhereOrderInput[A] =
    WhereOrderInput(EQ = a.some)

  def IN[A: Order](lst: List[A]): WhereOrderInput[A] =
    WhereOrderInput(IN = lst.some)

  implicit val customConfig: Configuration =
    Configuration.default.withDefaults

  implicit def DecoderWhereOrderInput[A: Decoder: Order]: Decoder[WhereOrderInput[A]] =
    deriveConfiguredDecoder[WhereOrderInput[A]]

}

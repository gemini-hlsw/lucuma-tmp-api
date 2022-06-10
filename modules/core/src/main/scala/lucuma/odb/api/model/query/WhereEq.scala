// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.query

import cats.Eq
import cats.syntax.eq._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

final case class WhereEq[A: Eq](
  EQ:  Option[A],
  NEQ: Option[A],
  IN:  Option[List[A]],
  NIN: Option[List[A]]
) extends WherePredicate[A] {

  def matches(a: A): Boolean =
    EQ.forall(_ === a)           &&
      NEQ.forall(_ =!= a)        &&
      IN.forall(_.contains(a))   &&
      NIN.forall(!_.contains(a))

}

object WhereEq {

  implicit def DecoderWhereEq[A: Decoder: Eq]: Decoder[WhereEq[A]] =
    deriveDecoder[WhereEq[A]]

}

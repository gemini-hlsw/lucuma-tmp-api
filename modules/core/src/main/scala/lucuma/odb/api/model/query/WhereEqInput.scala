// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.query

import cats.Eq
import cats.syntax.eq._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

final case class WhereEqInput[A: Eq](
  EQ:  Option[A],
  NEQ: Option[A],
  IN:  Option[List[A]],
  NIN: Option[List[A]]
) extends WherePredicate[A] {

  override def matches(a: A): Boolean =
    EQ.forall(_ === a)           &&
      NEQ.forall(_ =!= a)        &&
      IN.forall(_.contains(a))   &&
      NIN.forall(!_.contains(a))

}

object WhereEqInput {

  implicit def DecoderWhereEqInput[A: Decoder: Eq]: Decoder[WhereEqInput[A]] =
    deriveDecoder[WhereEqInput[A]]

}

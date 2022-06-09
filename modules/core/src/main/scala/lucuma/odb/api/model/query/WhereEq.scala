// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.query

import cats.Eq
import cats.syntax.eq._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

final case class WhereEq[A: Eq](
  eq:  Option[A],
  neq: Option[A],
  in:  Option[List[A]],
  nin: Option[List[A]]
) extends WherePredicate[A] {

  def matches(a: A): Boolean =
    eq.forall(_ === a)           &&
      neq.forall(_ =!= a)        &&
      in.forall(_.contains(a))   &&
      nin.forall(!_.contains(a))

}

object WhereEq {

  implicit def DecoderWhereEq[A: Decoder: Eq]: Decoder[WhereEq[A]] =
    deriveDecoder[WhereEq[A]]

}

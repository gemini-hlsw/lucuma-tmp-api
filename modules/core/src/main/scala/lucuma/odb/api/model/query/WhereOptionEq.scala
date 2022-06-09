// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.query

import cats.Eq
import cats.syntax.eq._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder


final case class WhereOptionEq[A: Eq](
  eq:     Option[A],
  neq:    Option[A],
  in:     Option[List[A]],
  nin:    Option[List[A]],
  isNull: Option[Boolean]
) extends WherePredicate[Option[A]] {

  def matches(a: Option[A]): Boolean = {

    def whenEmpty: Boolean =
      eq.isEmpty && in.forall(_.isEmpty) && nin.forall(_.nonEmpty)

    isNull.forall(_ === a.isEmpty)                 &&
      a.fold(whenEmpty)(WhereEq(eq, neq, in, nin).matches)
  }

}

object WhereOptionEq {

  implicit def DecoderWhereOptionEq[A: Decoder: Eq]: Decoder[WhereOptionEq[A]] =
    deriveDecoder[WhereOptionEq[A]]

}

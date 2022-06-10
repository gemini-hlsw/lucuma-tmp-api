// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.query

import cats.Eq
import cats.syntax.eq._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder


final case class WhereOptionEq[A: Eq](
  EQ:      Option[A],
  NEQ:     Option[A],
  IN:      Option[List[A]],
  NIN:     Option[List[A]],
  IS_NULL: Option[Boolean]
) extends WherePredicate[Option[A]] {

  def matches(a: Option[A]): Boolean = {

    def whenEmpty: Boolean =
      EQ.isEmpty && IN.forall(_.isEmpty) && NIN.forall(_.nonEmpty)

    IS_NULL.forall(_ === a.isEmpty)                 &&
      a.fold(whenEmpty)(WhereEq(EQ, NEQ, IN, NIN).matches)
  }

}

object WhereOptionEq {

  implicit def DecoderWhereOptionEq[A: Decoder: Eq]: Decoder[WhereOptionEq[A]] =
    deriveDecoder[WhereOptionEq[A]]

}

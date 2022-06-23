// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.query

import cats.Eq
import cats.syntax.eq._
import cats.syntax.option._
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredDecoder
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax.EncoderOps
import lucuma.core.util.Enumerated

final case class WhereEqInput[A: Eq](
  EQ:  Option[A]       = none,
  NEQ: Option[A]       = none,
  IN:  Option[List[A]] = none,
  NIN: Option[List[A]] = none
) extends WherePredicate[A] {

  override def matches(a: A): Boolean =
    EQ.forall(_ === a)           &&
      NEQ.forall(_ =!= a)        &&
      IN.forall(_.contains(a))   &&
      NIN.forall(!_.contains(a))

}

object WhereEqInput {

  def MatchAll[A: Eq]: WhereEqInput[A] =
    WhereEqInput[A]()

  def EQ[A: Eq](a: A): WhereEqInput[A] =
    WhereEqInput[A](EQ = a.some)

  def ANY[A: Enumerated]: WhereEqInput[A] =
    WhereEqInput(IN = Enumerated[A].all.some)

  implicit val customConfig: Configuration =
    Configuration.default.withDefaults

  implicit def DecoderWhereEqInput[A: Decoder: Eq]: Decoder[WhereEqInput[A]] =
    deriveConfiguredDecoder[WhereEqInput[A]]

  // Need an encoder so we can define a default value for the WhereEqInput[Existence]
  // property. Inexplicably automatic derivation doesn't work.
  implicit def EncoderWhereEqInput[A: Encoder]: Encoder[WhereEqInput[A]] =
    (a: WhereEqInput[A]) => Json.fromFields(
      a.EQ.map(v => "EQ" -> v.asJson).toList     ++
        a.NEQ.map(v => "NEQ" -> v.asJson).toList ++
        a.IN.map(vs => "IN" -> vs.asJson).toList ++
        a.NIN.map(vs => "NIN" -> vs.asJson).toList
    )

}

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.core.model.{IntPercent, Partner}
import lucuma.odb.api.model.query.{WhereCombinator, WhereEqInput, WhereOrderInput}

final case class WhereProposalPartnerEntryInput(
  AND:     Option[List[WhereProposalPartnerEntryInput]] = None,
  OR:      Option[List[WhereProposalPartnerEntryInput]] = None,
  NOT:     Option[WhereProposalPartnerEntryInput]       = None,

  partner: Option[WhereEqInput[Partner]]                = None,
  percent: Option[WhereOrderInput[Int]]                 = None
) extends WhereCombinator[Map[Partner, IntPercent]] {

  override def matches(a: Map[Partner, IntPercent]): Boolean = {
    val aʹ = a.filter { case (_, p) => p.value > 0 }

    combinatorMatches(aʹ)                    &&
      aʹ.exists { case (k, v) =>
        partner.forall(_.matches(k))         &&
        percent.forall(_.matches(v.value))
      }
  }

}

object WhereProposalPartnerEntryInput {

  implicit val DecoderWhereProposalPartnerEntryInput: Decoder[WhereProposalPartnerEntryInput] =
    deriveDecoder[WhereProposalPartnerEntryInput]

}

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import io.circe.Decoder
import lucuma.core.`enum`.{TacCategory, ToOActivation}
import lucuma.core.model.Proposal
import lucuma.odb.api.model.query.{WhereCombinator, WhereEq, WhereOption, WhereOptionEq, WhereOptionString, WherePredicate}


final case class WhereProposal(
  AND:           Option[List[WhereProposal]],
  OR:            Option[List[WhereProposal]],
  NOT:           Option[WhereProposal],
  IS_NULL:       Option[Boolean],

  title:         Option[WhereOptionString],
  category:      Option[WhereOptionEq[TacCategory]],
  toOActivation: Option[WhereEq[ToOActivation]],
  abstrakt:      Option[WhereOptionString]
) extends WhereOption[Proposal] with WhereCombinator[Option[Proposal]] {

  override def whenEmpty: Boolean =
    title.isEmpty && category.isEmpty && toOActivation.isEmpty && abstrakt.isEmpty

  override def whenNonEmpty: WherePredicate[Proposal] =
    new WherePredicate[Proposal] {
      override def matches(a: Proposal): Boolean =
        title.forall(_.matchesNonEmptyString(a.title))     &&
          category.forall(_.matches(a.category))           &&
          toOActivation.forall(_.matches(a.toOActivation)) &&
          abstrakt.forall(_.matchesNonEmptyString(a.abstrakt))
    }

}

object WhereProposal {

  import io.circe.generic.extras.semiauto._
  import io.circe.generic.extras.Configuration
  implicit val customConfig: Configuration =
    Configuration.default.withDefaults
      .copy(transformMemberNames = {
         case "abstrakt" => "abstract"
          case other => other
        })


  implicit val DecoderProposalWhere: Decoder[WhereProposal] =
    deriveConfiguredDecoder[WhereProposal]

}

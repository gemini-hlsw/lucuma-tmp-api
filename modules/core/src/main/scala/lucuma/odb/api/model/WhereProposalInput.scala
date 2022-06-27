// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.kernel.Eq
import io.circe.Decoder
import lucuma.core.enums.{TacCategory, ToOActivation}
import lucuma.core.model.Proposal
import lucuma.odb.api.model.query.{WhereCombinator, WhereEqInput, WhereOption, WhereOptionEqInput, WhereOptionStringInput, WherePredicate}


final case class WhereProposalInput(
  IS_NULL:       Option[Boolean]                         = None,
  AND:           Option[List[WhereProposalInput]]        = None,
  OR:            Option[List[WhereProposalInput]]        = None,
  NOT:           Option[WhereProposalInput]              = None,

  title:         Option[WhereOptionStringInput]          = None,
  clazz:         Option[WhereProposalClassInput]         = None,
  category:      Option[WhereOptionEqInput[TacCategory]] = None,
  toOActivation: Option[WhereEqInput[ToOActivation]]     = None,
  abstrakt:      Option[WhereOptionStringInput]          = None,
  partners:      Option[WhereProposalPartnersInput]      = None
) extends WhereOption[Proposal] with WhereCombinator[Option[Proposal]] {

  override def allEmpty: Boolean =
    title.isEmpty           &&
      clazz.isEmpty         &&
      category.isEmpty      &&
      toOActivation.isEmpty &&
      abstrakt.isEmpty      &&
      partners.isEmpty

  override def whenNonEmpty: WherePredicate[Proposal] =
    (a: Proposal) =>
      title.forall(_.matchesNonEmptyString(a.title)) &&
      clazz.forall(_.matches(a.proposalClass)) &&
      category.forall(_.matches(a.category)) &&
      toOActivation.forall(_.matches(a.toOActivation)) &&
      abstrakt.forall(_.matchesNonEmptyString(a.abstrakt))

  override def matches(a: Option[Proposal]): Boolean =
    optionMatches(a) && combinatorMatches(a)
}

object WhereProposalInput {

  import io.circe.generic.extras.semiauto._
  import io.circe.generic.extras.Configuration
  implicit val customConfig: Configuration =
    Configuration.default.withDefaults
      .copy(transformMemberNames = {
         case "abstrakt" => "abstract"
         case "clazz"    => "class"
         case other      => other
      })


  implicit val DecoderWhereProposalInput: Decoder[WhereProposalInput] =
    deriveConfiguredDecoder[WhereProposalInput]

  implicit val EqWhereProposalInput: Eq[WhereProposalInput] =
    Eq.fromUniversalEquals
}

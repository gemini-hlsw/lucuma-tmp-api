// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.syntax.eq._
import io.circe.Decoder
import lucuma.core.`enum`.{TacCategory, ToOActivation}
import lucuma.core.model.Proposal
import lucuma.odb.api.model.query.{WhereCombinator, WhereEq, WhereOptionEq, WhereOptionString}


final case class WhereProposal(
  title:         Option[WhereOptionString],
  category:      Option[WhereOptionEq[TacCategory]],
  toOActivation: Option[WhereEq[ToOActivation]],
  abstrakt:      Option[WhereOptionString],
  isNull:        Option[Boolean],

  and:           Option[List[WhereProposal]],
  or:            Option[List[WhereProposal]],
  not:           Option[WhereProposal]
) extends WhereCombinator[Option[Proposal]] {

    override def matches(a: Option[Proposal]): Boolean = {

      def whenEmpty: Boolean =
        title.isEmpty && category.isEmpty && toOActivation.isEmpty && abstrakt.isEmpty

      isNull.forall(_ === a.isEmpty) &&
        a.fold(whenEmpty) { aʹ =>
          title.forall(_.matchesNonEmpty(aʹ.title))           &&
            category.forall(_.matches(aʹ.category))           &&
            toOActivation.forall(_.matches(aʹ.toOActivation)) &&
            abstrakt.forall(_.matchesNonEmpty(aʹ.abstrakt))
        } && combinatorMatch(a)
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

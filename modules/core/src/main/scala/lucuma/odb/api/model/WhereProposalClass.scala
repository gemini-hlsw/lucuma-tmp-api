// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import io.circe.Decoder
import lucuma.core.model.ProposalClass
import lucuma.odb.api.model.query.{WhereEq, WhereOrder, WherePredicate}

final case class WhereProposalClass(
  classType:  Option[WhereEq[ProposalClassEnum]],
  minPercent: Option[WhereOrder[Int]]
) extends WherePredicate[ProposalClass] {

  override def matches(a: ProposalClass): Boolean =
    super.matches(a)                                                      &&
      classType.forall(_.matches(ProposalClassEnum.fromProposalClass(a))) &&
      minPercent.forall(_.matches(a.minPercentTime.value))

}

object WhereProposalClass {

  import io.circe.generic.extras.semiauto._
  import io.circe.generic.extras.Configuration
  implicit val customConfig: Configuration =
    Configuration.default.withDefaults
      .copy(transformMemberNames = {
         case "classType" => "type"
          case other      => other
      })

  implicit val DecoderWhereProposalClass: Decoder[WhereProposalClass] =
    deriveConfiguredDecoder[WhereProposalClass]

}

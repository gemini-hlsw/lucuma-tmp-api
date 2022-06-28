// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import io.circe.Decoder
import lucuma.core.model.ProposalClass
import lucuma.odb.api.model.query.{WhereEqInput, WhereOrderInput, WherePredicate}

final case class WhereProposalClassInput(
  classType:  Option[WhereEqInput[ProposalClassEnum]] = None,
  minPercent: Option[WhereOrderInput[Int]]            = None
) extends WherePredicate[ProposalClass] {

  override def matches(a: ProposalClass): Boolean =
    classType.forall(_.matches(ProposalClassEnum.fromProposalClass(a))) &&
      minPercent.forall(_.matches(a.minPercentTime.value))

}

object WhereProposalClassInput {

  import io.circe.generic.extras.semiauto._
  import io.circe.generic.extras.Configuration
  implicit val customConfig: Configuration =
    Configuration.default.withDefaults
      .copy(transformMemberNames = {
         case "classType" => "type"
          case other      => other
      })

  implicit val DecoderWhereProposalClassInput: Decoder[WhereProposalClassInput] =
    deriveConfiguredDecoder[WhereProposalClassInput]

}

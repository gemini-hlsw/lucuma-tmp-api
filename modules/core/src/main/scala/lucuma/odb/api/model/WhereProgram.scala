// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.core.model.Program
import lucuma.odb.api.model.query.{WhereCombinator, WhereEq, WhereOptionString, WhereOrder}

final case class WhereProgram(
  id:        Option[WhereOrder[Program.Id]],
  name:      Option[WhereOptionString],
  existence: Option[WhereEq[Existence]],
  proposal:  Option[WhereProposal],

  and:       Option[List[WhereProgram]],
  or:        Option[List[WhereProgram]],
  not:       Option[WhereProgram]
) extends WhereCombinator[ProgramModel] {

  override def matches(a: ProgramModel): Boolean =
    id.forall(_.matches(a.id))                    &&
      name.forall(_.matches(a.name.map(_.value))) &&
      existence.forall(_.matches(a.existence))    &&
      proposal.forall(_.matches(a.proposal))      &&
      combinatorMatch(a)

}

object WhereProgram {

  implicit val DecoderProgramWhere: Decoder[WhereProgram] =
    deriveDecoder[WhereProgram]

}

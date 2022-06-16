// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.core.model.Program
import lucuma.odb.api.model.query.{WhereCombinator, WhereEqInput, WhereOptionStringInput, WhereOrderInput}

final case class WhereProgramInput(
  AND:       Option[List[WhereProgramInput]],
  OR:        Option[List[WhereProgramInput]],
  NOT:       Option[WhereProgramInput],

  id:        Option[WhereOrderInput[Program.Id]],
  name:      Option[WhereOptionStringInput],
  existence: Option[WhereEqInput[Existence]],
  proposal:  Option[WhereProposalInput]
) extends WhereCombinator[ProgramModel] {

  override def matches(a: ProgramModel): Boolean =
    combinatorMatches(a)                          &&
      id.forall(_.matches(a.id))                  &&
      name.forall(_.matches(a.name.map(_.value))) &&
      existence.forall(_.matches(a.existence))    &&
      proposal.forall(_.matches(a.proposal))

}

object WhereProgramInput {

  implicit val DecoderWhereProgramInput: Decoder[WhereProgramInput] =
    deriveDecoder[WhereProgramInput]

}

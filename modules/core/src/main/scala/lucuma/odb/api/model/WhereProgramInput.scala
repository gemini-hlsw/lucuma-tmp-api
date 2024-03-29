// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.syntax.option._
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredDecoder
import io.circe.Decoder
import lucuma.core.model.Program
import lucuma.odb.api.model.query.{WhereCombinator, WhereOptionStringInput, WhereOrderInput}

final case class WhereProgramInput(
  AND:       Option[List[WhereProgramInput]]     = None,
  OR:        Option[List[WhereProgramInput]]     = None,
  NOT:       Option[WhereProgramInput]           = None,

  id:        Option[WhereOrderInput[Program.Id]] = None,
  name:      Option[WhereOptionStringInput]      = None,
  proposal:  Option[WhereProposalInput]          = None
) extends WhereCombinator[ProgramModel] {

  override def matches(a: ProgramModel): Boolean =
    combinatorMatches(a)                          &&
      id.forall(_.matches(a.id))                  &&
      name.forall(_.matches(a.name.map(_.value))) &&
      proposal.forall(_.matches(a.proposal))

  def withId(id: Program.Id): WhereProgramInput =
    copy(id = WhereOrderInput.EQ(id).some)

  def withIds(ids: List[Program.Id]): WhereProgramInput =
    copy(id = WhereOrderInput.IN(ids).some)

}

object WhereProgramInput {

  val MatchAll: WhereProgramInput =
    WhereProgramInput()

  implicit val customConfig: Configuration =
    Configuration.default.withDefaults

  implicit val DecoderWhereProgramInput: Decoder[WhereProgramInput] =
    deriveConfiguredDecoder[WhereProgramInput]

  implicit val EqWhereProgramInput: Eq[WhereProgramInput] =
    Eq.fromUniversalEquals

}

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.syntax.option._
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredDecoder
//import io.circe.{Decoder, HCursor}
import io.circe.Decoder
//import io.circe.generic.extras.Configuration
import lucuma.core.model.Program
import lucuma.odb.api.model.query.{WhereCombinator, WhereEqInput, WhereOptionStringInput, WhereOrderInput}

final case class WhereProgramInput(
  AND:       Option[List[WhereProgramInput]]     = None,
  OR:        Option[List[WhereProgramInput]]     = None,
  NOT:       Option[WhereProgramInput]           = None,

  id:        Option[WhereOrderInput[Program.Id]] = None,
  name:      Option[WhereOptionStringInput]      = None,
  proposal:  Option[WhereProposalInput]          = None,
  existence: Option[WhereEqInput[Existence]]     = WhereEqInput.EQ(Existence.Present: Existence).some
) extends WhereCombinator[ProgramModel] {

  override def matches(a: ProgramModel): Boolean =
    combinatorMatches(a)                          &&
      id.forall(_.matches(a.id))                  &&
      name.forall(_.matches(a.name.map(_.value))) &&
      proposal.forall(_.matches(a.proposal))      &&
      existence.forall(_.matches(a.existence))

}

object WhereProgramInput {

  val MatchPresent: WhereProgramInput =
    WhereProgramInput()

  implicit val customConfig: Configuration =
    Configuration.default.withDefaults

  implicit val DecoderWhereProgramInput: Decoder[WhereProgramInput] =
/*
    (c: HCursor) => {
      println("\n\n\n\n******* SHIT ******\n\n\n\n")
      for {
        and       <- c.get[Option[List[WhereProgramInput]]]("AND")
        or        <- c.get[Option[List[WhereProgramInput]]]("OR")
        not       <- c.get[Option[WhereProgramInput]]("NOT")
        id        <- c.get[Option[WhereOrderInput[Program.Id]]]("id")
        name      <- c.get[Option[WhereOptionStringInput]]("name")
        proposal  <- c.get[Option[WhereProposalInput]]("proposal")
        existence <- c.getOrElse[WhereEqInput[Existence]]("existence")(WhereEqInput.EQ(Existence.Present: Existence)).map(_.some)
      } yield WhereProgramInput(
        and,
        or,
        not,
        id,
        name,
        proposal,
        existence
      )
    }

 */
    deriveConfiguredDecoder[WhereProgramInput]

}

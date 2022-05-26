// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import eu.timepit.refined.scalacheck.string._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.{Program, Proposal}
import lucuma.core.model.arb.ArbProposal
import lucuma.core.util.arb.{ArbEnumerated, ArbGid}
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary


trait ArbProgramModel {

  import ArbEnumerated._
  import ArbGid._
  import ArbProposal._
  import ArbProposalInput._

  implicit val arbProgramModel: Arbitrary[ProgramModel] =
    Arbitrary {
      for {
        id <- arbitrary[Program.Id]
        ex <- arbitrary[Existence]
        nm <- arbitrary[Option[NonEmptyString]]
        p  <- arbitrary[Option[Proposal]]
      } yield ProgramModel(id, ex, nm, p)
    }

  implicit val cogProgramModel: Cogen[ProgramModel] =
    Cogen[(
      Program.Id,
      Existence,
      Option[String],
      Option[Proposal]
    )].contramap { in => (
      in.id,
      in.existence,
      in.name.map(_.value),
      in.proposal
    )}

  implicit val arbProgramModelCreate: Arbitrary[ProgramModel.Create] =
    Arbitrary {
      for {
        id <- arbitrary[Option[Program.Id]]
        nm <- arbitrary[Option[NonEmptyString]]
        p  <- arbitrary[Option[ProposalInput]]
      } yield ProgramModel.Create(id, nm, p)
    }

  implicit val cogProgramModelCreate: Cogen[ProgramModel.Create] =
    Cogen[(
      Option[Program.Id],
      Option[String],
      Option[ProposalInput]
    )].contramap { in => (
      in.programId,
      in.name.map(_.value),
      in.proposal
    )}
}

object ArbProgramModel extends ArbProgramModel

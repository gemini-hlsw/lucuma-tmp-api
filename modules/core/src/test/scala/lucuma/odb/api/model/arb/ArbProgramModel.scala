// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import clue.data.Input
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
  import ArbInput._
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

  implicit val arbProgramModelPropertiesInput: Arbitrary[ProgramModel.PropertiesInput] =
    Arbitrary {
      for {
        nm <- arbitrary[Input[NonEmptyString]]
        p  <- arbitrary[Input[ProposalInput]]
        e  <- arbitrary[Input[Existence]]
      } yield ProgramModel.PropertiesInput(nm, p, e)
    }

  implicit val cogProgramModelPropertiesInput: Cogen[ProgramModel.PropertiesInput] =
    Cogen[(
      Input[String],
      Input[ProposalInput],
      Input[Existence]
    )].contramap { in => (
      in.name.map(_.value),
      in.proposal,
      in.existence
    )}

  implicit val arbProgramModelCreate: Arbitrary[ProgramModel.CreateInput] =
    Arbitrary {
      for {
        p  <- arbitrary[Option[ProgramModel.PropertiesInput]]
      } yield ProgramModel.CreateInput(p)
    }

  implicit val cogProgramModelCreate: Cogen[ProgramModel.CreateInput] =
    Cogen[(
      Option[ProgramModel.PropertiesInput]
    )].contramap(_.properties)
}

object ArbProgramModel extends ArbProgramModel

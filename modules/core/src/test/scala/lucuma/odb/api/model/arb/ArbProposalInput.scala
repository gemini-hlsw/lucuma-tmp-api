// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.arb

import clue.data.Input
import eu.timepit.refined.scalacheck.all._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enum.{TacCategory, ToOActivation}
import lucuma.core.model.{IntPercent, Partner}
import lucuma.core.util.arb.ArbEnumerated
import lucuma.odb.api.model.{ProposalClassInput, ProposalInput}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._
import lucuma.odb.api.model.PartnerSplit

trait ArbProposalInput {
  import ArbEnumerated._
  import ArbInput._
  import ArbProposalClassInput._
  import ProposalInput._

  implicit val arbPartnerSplit: Arbitrary[PartnerSplit] =
    Arbitrary {
      for {
        partner <- arbitrary[Partner]
        percent <- arbitrary[IntPercent]
      } yield PartnerSplit(partner, percent)
    }

  implicit val cogPartnerSplit: Cogen[PartnerSplit] =
    Cogen[(Partner, IntPercent)].contramap(ps => (ps.partner, ps.percent))

  implicit val arbPartnerSplitInput: Arbitrary[PartnerSplitInput] = 
    Arbitrary {
      for {
        partner <- arbitrary[Input[Partner]]
        percent <- arbitrary[Input[IntPercent]]
      } yield PartnerSplitInput(partner, percent)
    }

  implicit val cogPartnerSplitInput: Cogen[PartnerSplitInput] = 
    Cogen[(Input[Partner], Input[IntPercent])].contramap(ps => (ps.partner, ps.percent))

  implicit val arbProposalInput: Arbitrary[ProposalInput] = 
    Arbitrary {
      for {
        tit <- arbitrary[Input[NonEmptyString]]
        cls <- arbitrary[Input[ProposalClassInput]]
        cat <- arbitrary[Input[TacCategory]]
        act <- arbitrary[Input[ToOActivation]]
        abs <- arbitrary[Input[NonEmptyString]]
        ps  <- arbitrary[Input[List[PartnerSplitInput]]]
      } yield ProposalInput(tit, cls, cat, act, abs, ps)
    }

  implicit val cogProposalInput: Cogen[ProposalInput] =
    Cogen[(
      Input[NonEmptyString], 
      Input[ProposalClassInput], 
      Input[TacCategory], 
      Input[ToOActivation], 
      Input[NonEmptyString], 
      Input[List[PartnerSplitInput]]
    )].contramap{ p => (
      p.title,
      p.proposalClass,
      p.category,
      p.toOActivation,
      p.abstrakt,
      p.partnerSplits
    )}
}

object ArbProposalInput extends ArbProposalInput

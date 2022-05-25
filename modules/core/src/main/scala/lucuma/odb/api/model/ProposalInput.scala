// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats._
import cats.Order._
import cats.data.StateT
import cats.syntax.all._
import clue.data.Input
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.refined._
import lucuma.core.enum._
import lucuma.core.model.IntPercent
import lucuma.core.model.{Partner, Proposal}
import lucuma.odb.api.model.EitherInput
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.lens._
import lucuma.odb.api.model.syntax.validatedinput._
import monocle.Focus

import scala.collection.immutable.SortedMap

import ProposalInput._

final case class PartnerSplit(partner: Partner, percent: IntPercent)

object PartnerSplit {
  val partner = Focus[PartnerSplit](_.partner)
  val percent = Focus[PartnerSplit](_.percent)

  implicit val decoderPartnerSplit: Decoder[PartnerSplit] = deriveDecoder[PartnerSplit]

  implicit val eqPartnerSplit: Eq[PartnerSplit] = Eq.instance {
    case (PartnerSplit(a1, b1), PartnerSplit(a2, b2)) => a1 === a2 && b1 === b2
  }
}

final case class ProposalInput(
  title:         Input[NonEmptyString]          = Input.ignore,
  proposalClass: Input[ProposalClassInput]      = Input.ignore,
  category:      Input[TacCategory]             = Input.ignore,
  toOActivation: Input[ToOActivation]           = Input.ignore,
  abstrakt:      Input[NonEmptyString]          = Input.ignore,
  partnerSplits: Input[List[PartnerSplitInput]] = Input.ignore
) extends EditorInput[Proposal] {

  private def validatePartnerSplits(splits: List[PartnerSplitInput]): ValidatedInput[SortedMap[Partner, IntPercent]] = 
    splits.traverse(_.create).andThen { l =>
      val total = l.map(_.percent.value).sum
      // We need to accept empty list for initial creation - will need to validate before proposal submission.
      if (l.isEmpty || total === 100) {
        val map = SortedMap.from(l.map(ps => (ps.partner, ps.percent)))
        if (l.size === map.size)
          map.validNec
        else InputError.fromMessage("""Each partner can only appear once in "partnerSplits"""").invalidNec
      }
      else InputError.fromMessage(s""""partnerSplits" percents must add up to 100%. Current sum is $total""").invalidNec
    }

  override def create: lucuma.odb.api.model.ValidatedInput[Proposal] = {
    (proposalClass.notMissingAndThen("proposalClass")(_.create),
     toOActivation.notMissing("toOActivation"),
     partnerSplits.notMissingAndThen("partnerSplits")(validatePartnerSplits)
    ).mapN { case (pc, too, ps) => Proposal(title.toOption, pc, category.toOption, too, abstrakt.toOption, ps) }
  }
    
  override def edit: StateT[EitherInput,Proposal,Unit] = {
    def validateOptionalPartnerSplits(optSplits: Option[List[PartnerSplitInput]]): ValidatedInput[Option[SortedMap[Partner, IntPercent]]] =
      optSplits match {
        case None =>    none.validNec
        case Some(l) => validatePartnerSplits(l).map(_.some)
      }

    val validArgs =
      (toOActivation.validateIsNotNull("toOActivation"),
       partnerSplits.validateIsNotNull("partnerSplits").andThen(validateOptionalPartnerSplits)
      ).tupled

    for {
      args <- validArgs.liftState
      (too, ps) = args
      _ <- Proposal.title         := title.toOptionOption
      _ <- Proposal.proposalClass :! proposalClass
      _ <- Proposal.category      := category.toOptionOption
      _ <- Proposal.toOActivation := too
      _ <- Proposal.abstrakt      := abstrakt.toOptionOption
      _ <- Proposal.partnerSplits := ps
    } yield ()
  }
}

object ProposalInput {
  import io.circe.generic.extras.semiauto._
  import io.circe.generic.extras.Configuration
  implicit val customConfig: Configuration = 
    Configuration.default.withDefaults
      .copy(transformMemberNames = {
         case "abstrakt" => "abstract"
          case other => other
        })

  final case class PartnerSplitInput(
    partner: Input[Partner] =    Input.ignore,
    percent: Input[IntPercent] = Input.ignore
  ) extends EditorInput[PartnerSplit] {
    override def create: lucuma.odb.api.model.ValidatedInput[PartnerSplit] = 
      (partner.notMissing("partner"), 
       percent.notMissing("percent")
      ).mapN{ case (par, pct) => PartnerSplit(par, pct) }

    override def edit: StateT[EitherInput, PartnerSplit, Unit] = {
      val validArgs = 
        (partner.validateIsNotNull("partner"),
         percent.validateIsNotNull("percent")
        ).tupled

      for {
        args <- validArgs.liftState
        (par, pct) = args
        _ <- PartnerSplit.partner := par
        _ <- PartnerSplit.percent := pct
      } yield ()
    }
  }

  object PartnerSplitInput {
    implicit val DecoderPartnerSplitInput: Decoder[PartnerSplitInput] =
      deriveConfiguredDecoder[PartnerSplitInput]

    implicit val EqPartnerSplitInput: Eq[PartnerSplitInput] =
      Eq.by{ a => (a.partner, a.percent) }
  }

  implicit val DecoderProposalInput: Decoder[ProposalInput] =
    deriveConfiguredDecoder[ProposalInput]

  implicit val EqProposalInput: Eq[ProposalInput] =
    Eq.by { a => (a.title, a.proposalClass, a.category, a.toOActivation, a.abstrakt, a.partnerSplits) }
}

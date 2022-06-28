// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.data.NonEmptyList
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.auto._
import lucuma.core.model.ProposalClass
import lucuma.core.util.Enumerated

/**
 *
 */
sealed abstract class ProposalClassEnum(val tag: NonEmptyString) extends Product with Serializable

object ProposalClassEnum {

  case object Classical          extends ProposalClassEnum("Classical")
  case object DemoScience        extends ProposalClassEnum("DemoScience")
  case object DirectorsTime      extends ProposalClassEnum("DirectorsTime")
  case object Exchange           extends ProposalClassEnum("Exchange")
  case object FastTurnaround     extends ProposalClassEnum("FastTurnaround")
  case object Intensive          extends ProposalClassEnum("Intensive")
  case object LargeProgram       extends ProposalClassEnum("LargeProgram")
  case object PoorWeather        extends ProposalClassEnum("PoorWeather")
  case object Queue              extends ProposalClassEnum("Queue")
  case object SystemVerification extends ProposalClassEnum("SystemVerification")

  val all: NonEmptyList[ProposalClassEnum] =
    NonEmptyList.of(
      Classical,
      DemoScience,
      DirectorsTime,
      Exchange,
      FastTurnaround,
      Intensive,
      LargeProgram,
      PoorWeather,
      Queue,
      SystemVerification
    )

  implicit val EnumeratedProposalClassEnum: Enumerated[ProposalClassEnum] =
    Enumerated.fromNEL(all).withTag(_.tag)

  def fromProposalClass(pc: ProposalClass): ProposalClassEnum =
    pc match {
      case ProposalClass.Classical(_)          => Classical
      case ProposalClass.DemoScience(_)        => DemoScience
      case ProposalClass.DirectorsTime(_)      => DirectorsTime
      case ProposalClass.Exchange(_)           => Exchange
      case ProposalClass.FastTurnaround(_)     => FastTurnaround
      case ProposalClass.PoorWeather(_)        => PoorWeather
      case ProposalClass.Queue(_)              => Queue
      case ProposalClass.SystemVerification(_) => SystemVerification
      case ProposalClass.LargeProgram(_, _, _) => LargeProgram
      case ProposalClass.Intensive(_, _, _)    => Intensive
    }

}

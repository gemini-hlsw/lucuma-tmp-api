// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.model.ProposalClass
import lucuma.odb.api.repo.OdbCtx

import sangria.schema._

object ProposalClassSchema {

  import RefinedSchema.IntPercentType
  import TimeSchema.NonNegativeDurationType

  def ProposalClassType[F[_]]: InterfaceType[OdbCtx[F], ProposalClass] = 
    InterfaceType[OdbCtx[F], ProposalClass](
      name        = "ProposalClass",
      description = "Proposal Class interface",
      fields(basicFields[F, ProposalClass]())
    ).withPossibleTypes(() => List(
      PossibleObject[OdbCtx[F], ProposalClass](ClassicalProposalClassType),
      PossibleObject[OdbCtx[F], ProposalClass](DemoScienceProposalClassType),
      PossibleObject[OdbCtx[F], ProposalClass](DirectorsTimeProposalClassType),
      PossibleObject[OdbCtx[F], ProposalClass](ExchangeProposalClassType),
      PossibleObject[OdbCtx[F], ProposalClass](FastTurnaroundProposalClassType),
      PossibleObject[OdbCtx[F], ProposalClass](PoorWeatherProposalClassType),
      PossibleObject[OdbCtx[F], ProposalClass](QueueProposalClassType),
      PossibleObject[OdbCtx[F], ProposalClass](SystemVerificationProposalClassType),
      PossibleObject[OdbCtx[F], ProposalClass](LargeProgramProposalClassType),
      PossibleObject[OdbCtx[F], ProposalClass](IntensiveProposalClassType)
    ))

  private val basicDescription = "Minimum percent of requested observation time that is required"
  private def  basicFields[F[_], A <: ProposalClass](desc: String = basicDescription): Field[OdbCtx[F], A] =
    Field(
      name        = "minPercentTime",
      fieldType   = IntPercentType,
      description = Some(desc),
      resolve     = _.value.minPercentTime
    )

  def ClassicalProposalClassType[F[_]]: ObjectType[OdbCtx[F], ProposalClass.Classical] = 
    ObjectType(
      name        = "Classical",
      description = "Classical observing at Gemini",
      interfaces  = List(PossibleInterface[OdbCtx[F], ProposalClass.Classical](ProposalClassType[F])),
      fields      = Nil 
    )

  def DemoScienceProposalClassType[F[_]]: ObjectType[OdbCtx[F], ProposalClass.DemoScience] = 
    ObjectType(
      name        = "DemoScience",
      description = "Demo science",
      interfaces  = List(PossibleInterface[OdbCtx[F], ProposalClass.DemoScience](ProposalClassType[F])),
      fields      = Nil 
    )

  def DirectorsTimeProposalClassType[F[_]]: ObjectType[OdbCtx[F], ProposalClass.DirectorsTime] = 
    ObjectType(
      name        = "DirectorsTime",
      description = "Director's time",
      interfaces  = List(PossibleInterface[OdbCtx[F], ProposalClass.DirectorsTime](ProposalClassType[F])),
      fields      = Nil 
    )

  def ExchangeProposalClassType[F[_]]: ObjectType[OdbCtx[F], ProposalClass.Exchange] = 
    ObjectType(
      name        = "Exchange",
      description = "Exchange observing at Keck/Subaru",
      interfaces  = List(PossibleInterface[OdbCtx[F], ProposalClass.Exchange](ProposalClassType[F])),
      fields      = Nil 
    )

  def FastTurnaroundProposalClassType[F[_]]: ObjectType[OdbCtx[F], ProposalClass.FastTurnaround] = 
    ObjectType(
      name        = "FastTurnaround",
      description = "Fast turnaround observing at Gemini",
      interfaces  = List(PossibleInterface[OdbCtx[F], ProposalClass.FastTurnaround](ProposalClassType[F])),
      fields      = Nil 
    )

  def PoorWeatherProposalClassType[F[_]]: ObjectType[OdbCtx[F], ProposalClass.PoorWeather] = 
    ObjectType(
      name        = "PoorWeather",
      description = "Poor Weather",
      interfaces  = List(PossibleInterface[OdbCtx[F], ProposalClass.PoorWeather](ProposalClassType[F])),
      fields      = Nil
    )

  def QueueProposalClassType[F[_]]: ObjectType[OdbCtx[F], ProposalClass.Queue] =
    ObjectType(
      name        = "Queue",
      description = "Queue observing at Gemini",
      interfaces  = List(PossibleInterface[OdbCtx[F], ProposalClass.Queue](ProposalClassType[F])),
      fields      = Nil 
    )

  def SystemVerificationProposalClassType[F[_]]: ObjectType[OdbCtx[F], ProposalClass.SystemVerification] = 
    ObjectType(
      name        = "SystemVerification",
      description = "System Verification",
      interfaces  = List(PossibleInterface[OdbCtx[F], ProposalClass.SystemVerification](ProposalClassType[F])),
      fields      = Nil 
    )

  def LargeProgramProposalClassType[F[_]]: ObjectType[OdbCtx[F], ProposalClass.LargeProgram] =
    ObjectType(
      name        = "LargeProgram",
      description = "Large program observing at Gemini",
      interfaces  = List(PossibleInterface[OdbCtx[F], ProposalClass.LargeProgram](ProposalClassType[F])),
      fields      = 
        List(
          basicFields[F, ProposalClass.LargeProgram]("Minimum percent of time requested for this semester that is required"),
          Field(
            name        = "minPercentTotalTime",
            fieldType   = IntPercentType,
              description = Some("Minimum percent of total program time that is required"),
            resolve     = _.value.minPercentTotalTime
          ),
          Field(
            name        = "totalTime",
            fieldType   = NonNegativeDurationType,
              description = Some("Estimated total program time"),
            resolve     = _.value.totalTime
          )
        )
    )

  def IntensiveProposalClassType[F[_]]: ObjectType[OdbCtx[F], ProposalClass.Intensive] =
    ObjectType(
      name        = "Intensive",
      description = "Intensive program observing at Subaru",
      interfaces  = List(PossibleInterface[OdbCtx[F], ProposalClass.Intensive](ProposalClassType[F])),
      fields      = 
        List(
          basicFields[F, ProposalClass.Intensive]("Minimum percent of time requested for this semester that is required"),
          Field(
            name        = "minPercentTotalTime",
            fieldType   = IntPercentType,
              description = Some("Minimum percent of total program time that is required"),
            resolve     = _.value.minPercentTotalTime
          ),
          Field(
            name        = "totalTime",
            fieldType   = NonNegativeDurationType,
              description = Some("Estimated total program time"),
            resolve     = _.value.totalTime
          )
        )
    )
}

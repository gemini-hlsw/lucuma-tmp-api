// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.arb

import clue.data.Input
import eu.timepit.refined.scalacheck.all._
import lucuma.core.model.IntPercent
import lucuma.odb.api.model.ProposalClassInput
import lucuma.odb.api.model.DurationModel.NonNegDurationInput
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

trait ArbProposalClassInput {
  import ArbDurationModel._
  import ArbInput._
  import ProposalClassInput._

  implicit val arbClassicalInput: Arbitrary[ClassicalInput] =
    Arbitrary(arbitrary[Input[IntPercent]].map(ClassicalInput(_)))
  
  implicit val cogClassicalInput: Cogen[ClassicalInput] = 
    Cogen[Input[IntPercent]].contramap(_.minPercentTime)

  implicit val arbDemoScienceInput: Arbitrary[DemoScienceInput] =
    Arbitrary(arbitrary[Input[IntPercent]].map(DemoScienceInput(_)))
  
  implicit val cogDemoScienceInput: Cogen[DemoScienceInput] = 
    Cogen[Input[IntPercent]].contramap(_.minPercentTime)

  implicit val arbDirectorsTimeInput: Arbitrary[DirectorsTimeInput] =
    Arbitrary(arbitrary[Input[IntPercent]].map(DirectorsTimeInput(_)))
  
  implicit val cogDirectorsTimeInput: Cogen[DirectorsTimeInput] = 
    Cogen[Input[IntPercent]].contramap(_.minPercentTime)

  implicit val arbExchangeInput: Arbitrary[ExchangeInput] =
    Arbitrary(arbitrary[Input[IntPercent]].map(ExchangeInput(_)))
  
  implicit val cogExchangeInput: Cogen[ExchangeInput] = 
    Cogen[Input[IntPercent]].contramap(_.minPercentTime)

  implicit val arbFastTurnaroundInput: Arbitrary[FastTurnaroundInput] =
    Arbitrary(arbitrary[Input[IntPercent]].map(FastTurnaroundInput(_)))
  
  implicit val cogFastTurnaroundInput: Cogen[FastTurnaroundInput] = 
    Cogen[Input[IntPercent]].contramap(_.minPercentTime)

  implicit val arbPoorWeatherInput: Arbitrary[PoorWeatherInput] =
    Arbitrary(arbitrary[Input[IntPercent]].map(PoorWeatherInput(_)))
  
  implicit val cogPoorWeatherInput: Cogen[PoorWeatherInput] = 
    Cogen[Input[IntPercent]].contramap(_.minPercentTime)

  implicit val arbQueueInput: Arbitrary[QueueInput] =
    Arbitrary(arbitrary[Input[IntPercent]].map(QueueInput(_)))
  
  implicit val cogQueueInput: Cogen[QueueInput] = 
    Cogen[Input[IntPercent]].contramap(_.minPercentTime)

  implicit val arbSystemVerificationInput: Arbitrary[SystemVerificationInput] =
    Arbitrary(arbitrary[Input[IntPercent]].map(SystemVerificationInput(_)))
  
  implicit val cogSystemVerificationInput: Cogen[SystemVerificationInput] = 
    Cogen[Input[IntPercent]].contramap(_.minPercentTime)

  implicit val arbLargeProgramInput: Arbitrary[LargeProgramInput] =
    Arbitrary {
      for {
        minPct    <- arbitrary[Input[IntPercent]]
        minTotPct <- arbitrary[Input[IntPercent]]
        totalTime <- arbitrary[Input[NonNegDurationInput]]
      }  yield LargeProgramInput(minPct, minTotPct, totalTime)
    } 
  
  implicit val cogLargeProgramInput: Cogen[LargeProgramInput] = 
    Cogen[(Input[IntPercent], Input[IntPercent], Input[NonNegDurationInput])]
      .contramap(p => (p.minPercentTime, p.minPercentTotalTime, p.totalTime))

  implicit val arbIntensiveInput: Arbitrary[IntensiveInput] =
    Arbitrary {
      for {
        minPct    <- arbitrary[Input[IntPercent]]
        minTotPct <- arbitrary[Input[IntPercent]]
        totalTime <- arbitrary[Input[NonNegDurationInput]]
      }  yield IntensiveInput(minPct, minTotPct, totalTime)
    } 
  
  implicit val cogIntensiveInput: Cogen[IntensiveInput] = 
    Cogen[(Input[IntPercent], Input[IntPercent], Input[NonNegDurationInput])]
      .contramap(p => (p.minPercentTime, p.minPercentTotalTime, p.totalTime))

  implicit val arbProposalClassInput: Arbitrary[ProposalClassInput] =
    Arbitrary { 
      for { 
        cl <- arbitrary[Input[ClassicalInput]]
        de <- arbitrary[Input[DemoScienceInput]]
        di <- arbitrary[Input[DirectorsTimeInput]]
        ex <- arbitrary[Input[ExchangeInput]]
        fa <- arbitrary[Input[FastTurnaroundInput]]
        pw <- arbitrary[Input[PoorWeatherInput]]
        qu <- arbitrary[Input[QueueInput]]
        sv <- arbitrary[Input[SystemVerificationInput]]
        lp <- arbitrary[Input[LargeProgramInput]]
        in <- arbitrary[Input[IntensiveInput]]
      } yield ProposalClassInput(cl, de, di, ex, fa, pw, qu, sv, lp, in)
    }
  
  implicit val cogProposalClassInput: Cogen[ProposalClassInput] =
    Cogen[
      (
        Input[ClassicalInput],
        Input[DemoScienceInput],
        Input[DirectorsTimeInput],
        Input[ExchangeInput],
        Input[FastTurnaroundInput],
        Input[PoorWeatherInput],
        Input[QueueInput],
        Input[SystemVerificationInput],
        Input[LargeProgramInput],
        Input[IntensiveInput]
      )
    ].contramap(p =>
      (
        p.classical,
        p.demoScience,
        p.directorsTime,
        p.exchange,
        p.fastTurnaround,
        p.poorWeather,
        p.queue,
        p.systemVerification,
        p.largeProgram,
        p.intensive
      )
    )
}

object ArbProposalClassInput extends ArbProposalClassInput

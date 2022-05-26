// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.data.StateT
import cats.syntax.all._
import clue.data.Input
import io.circe.refined._
import eu.timepit.refined.cats._
import io.circe.Decoder
import lucuma.core.model.{IntPercent, ProposalClass}
import lucuma.core.model.ProposalClass._
import lucuma.odb.api.model.EitherInput
import lucuma.odb.api.model.DurationModel.NonNegDurationInput
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.lens._
import lucuma.odb.api.model.syntax.validatedinput._

import ProposalClassInput._

final case class ProposalClassInput(
  classical:          Input[ClassicalInput]          = Input.ignore,
  demoScience:        Input[DemoScienceInput]        = Input.ignore,
  directorsTime:      Input[DirectorsTimeInput]      = Input.ignore,
  exchange:           Input[ExchangeInput]           = Input.ignore,
  fastTurnaround:     Input[FastTurnaroundInput]     = Input.ignore,
  poorWeather:        Input[PoorWeatherInput]        = Input.ignore,
  queue:              Input[QueueInput]              = Input.ignore,
  systemVerification: Input[SystemVerificationInput] = Input.ignore,
  largeProgram:       Input[LargeProgramInput]       = Input.ignore,
  intensive:          Input[IntensiveInput]          = Input.ignore
) extends EditorInput[ProposalClass] {
  override def create: lucuma.odb.api.model.ValidatedInput[ProposalClass] =
    ValidatedInput.requireOne(
      "proposalClass",
      classical.map(_.create).toOption,
      demoScience.map(_.create).toOption,
      directorsTime.map(_.create).toOption,
      exchange.map(_.create).toOption,
      fastTurnaround.map(_.create).toOption,
      poorWeather.map(_.create).toOption,
      queue.map(_.create).toOption,
      systemVerification.map(_.create).toOption,
      largeProgram.map(_.create).toOption,
      intensive.map(_.create).toOption,
    )

  override def edit: StateT[EitherInput,ProposalClass,Unit] = 
    EditorInput.editOneOf(
      ("classical",          classical,          ProposalClass.classical),
      ("demoScience",        demoScience,        ProposalClass.demoScience),
      ("directorsTime",      directorsTime,      ProposalClass.directorsTime),
      ("exchange",           exchange,           ProposalClass.exchange),
      ("fastTurnaround",     fastTurnaround,     ProposalClass.fastTurnaround),
      ("poorWeather",        poorWeather,        ProposalClass.poorWeather),
      ("queue",              queue,              ProposalClass.queue),
      ("systemVerification", systemVerification, ProposalClass.systemVerification),
      ("largeProgram",       largeProgram,       ProposalClass.largeProgram),
      ("intensive",          intensive,          ProposalClass.intensive)
    )
}

object ProposalClassInput {
  import io.circe.generic.extras.semiauto._
  import io.circe.generic.extras.Configuration
  implicit val customConfig: Configuration = Configuration.default.withDefaults

  implicit val DecoderProposalClassInput: Decoder[ProposalClassInput] =
    deriveConfiguredDecoder[ProposalClassInput]

  implicit val EqProposalClassInput: Eq[ProposalClassInput] =
    Eq.by { a =>
      (
        a.classical,
        a.demoScience,
        a.directorsTime,
        a.exchange,
        a.fastTurnaround,
        a.poorWeather,
        a.queue,
        a.systemVerification,
        a.largeProgram,
        a.intensive
      )
    }

  final case class ClassicalInput(
    minPercentTime: Input[IntPercent] = Input.ignore
  ) extends EditorInput[Classical] {
    override def create: lucuma.odb.api.model.ValidatedInput[Classical] = 
      minPercentTime.notMissing("minPercentTime").map(Classical(_))
  
    override def edit: StateT[EitherInput, Classical,Unit] = 
      for {
        minPct <- minPercentTime.validateIsNotNull("minPercentTime").liftState
        _      <- Classical.minPercentTime := minPct
      } yield ()
  }

  object ClassicalInput {
    implicit val DecoderClassicalInput: Decoder[ClassicalInput] =
      deriveConfiguredDecoder[ClassicalInput]

    implicit val EqClassicalInput: Eq[ClassicalInput] =
      Eq.by { a => a.minPercentTime }
  }

  final case class DemoScienceInput(
    minPercentTime: Input[IntPercent] = Input.ignore
  ) extends EditorInput[DemoScience] {
    override def create: lucuma.odb.api.model.ValidatedInput[DemoScience] = 
      minPercentTime.notMissing("minPercentTime").map(DemoScience(_))
  
    override def edit: StateT[EitherInput, DemoScience,Unit] = 
      for {
        minPct <- minPercentTime.validateIsNotNull("minPercentTime").liftState
        _      <- DemoScience.minPercentTime := minPct
      } yield ()
  }

  object DemoScienceInput {
    implicit val DecoderDemoScienceInput: Decoder[DemoScienceInput] =
      deriveConfiguredDecoder[DemoScienceInput]

    implicit val EqDemoScienceInput: Eq[DemoScienceInput] =
      Eq.by { a => a.minPercentTime }
  }

  final case class DirectorsTimeInput(
    minPercentTime: Input[IntPercent] = Input.ignore
  ) extends EditorInput[DirectorsTime] {
    override def create: lucuma.odb.api.model.ValidatedInput[DirectorsTime] = 
      minPercentTime.notMissing("minPercentTime").map(DirectorsTime(_))
  
    override def edit: StateT[EitherInput, DirectorsTime,Unit] = 
      for {
        minPct <- minPercentTime.validateIsNotNull("minPercentTime").liftState
        _      <- DirectorsTime.minPercentTime := minPct
      } yield ()
  }

  object DirectorsTimeInput {
    implicit val DecoderDirectorsTimeInput: Decoder[DirectorsTimeInput] =
      deriveConfiguredDecoder[DirectorsTimeInput]

    implicit val EqDirectorsTimeInput: Eq[DirectorsTimeInput] =
      Eq.by { a => a.minPercentTime }
  }

  final case class ExchangeInput(
    minPercentTime: Input[IntPercent] = Input.ignore
  ) extends EditorInput[Exchange] {
    override def create: lucuma.odb.api.model.ValidatedInput[Exchange] = 
      minPercentTime.notMissing("minPercentTime").map(Exchange(_))
  
    override def edit: StateT[EitherInput, Exchange,Unit] = 
      for {
        minPct <- minPercentTime.validateIsNotNull("minPercentTime").liftState
        _      <- Exchange.minPercentTime := minPct
      } yield ()
  }

  object ExchangeInput {
    implicit val DecoderExchangeInput: Decoder[ExchangeInput] =
      deriveConfiguredDecoder[ExchangeInput]

    implicit val EqExchangeInput: Eq[ExchangeInput] =
      Eq.by { a => a.minPercentTime }
  }

  final case class FastTurnaroundInput(
    minPercentTime: Input[IntPercent] = Input.ignore
  ) extends EditorInput[FastTurnaround] {
    override def create: lucuma.odb.api.model.ValidatedInput[FastTurnaround] = 
      minPercentTime.notMissing("minPercentTime").map(FastTurnaround(_))
  
    override def edit: StateT[EitherInput, FastTurnaround,Unit] = 
      for {
        minPct <- minPercentTime.validateIsNotNull("minPercentTime").liftState
        _      <- FastTurnaround.minPercentTime := minPct
      } yield ()
  }

  object FastTurnaroundInput {
    implicit val DecoderFastTurnaroundInput: Decoder[FastTurnaroundInput] =
      deriveConfiguredDecoder[FastTurnaroundInput]

    implicit val EqFastTurnaroundInput: Eq[FastTurnaroundInput] =
      Eq.by { a => a.minPercentTime }
  }

  final case class PoorWeatherInput(
    minPercentTime: Input[IntPercent] = Input.ignore
  ) extends EditorInput[PoorWeather] {
    override def create: lucuma.odb.api.model.ValidatedInput[PoorWeather] = 
      minPercentTime.notMissing("minPercentTime").map(PoorWeather(_))
  
    override def edit: StateT[EitherInput, PoorWeather,Unit] = 
      for {
        minPct <- minPercentTime.validateIsNotNull("minPercentTime").liftState
        _      <- PoorWeather.minPercentTime := minPct
      } yield ()
  }

  object PoorWeatherInput {
    implicit val DecoderPoorWeatherInput: Decoder[PoorWeatherInput] =
      deriveConfiguredDecoder[PoorWeatherInput]

    implicit val EqPoorWeatherInput: Eq[PoorWeatherInput] =
      Eq.by { a => a.minPercentTime }
  }

  final case class QueueInput(
    minPercentTime: Input[IntPercent] = Input.ignore
  ) extends EditorInput[Queue] {
    override def create: lucuma.odb.api.model.ValidatedInput[Queue] = 
      minPercentTime.notMissing("minPercentTime").map(Queue(_))
  
    override def edit: StateT[EitherInput, Queue,Unit] = 
      for {
        minPct <- minPercentTime.validateIsNotNull("minPercentTime").liftState
        _      <- Queue.minPercentTime := minPct
      } yield ()
  }

  object QueueInput {
    implicit val DecoderQueueInput: Decoder[QueueInput] =
      deriveConfiguredDecoder[QueueInput]

    implicit val EqQueueInput: Eq[QueueInput] =
      Eq.by { a => a.minPercentTime }
  }

  final case class SystemVerificationInput(
    minPercentTime: Input[IntPercent] = Input.ignore
  ) extends EditorInput[SystemVerification] {
    override def create: lucuma.odb.api.model.ValidatedInput[SystemVerification] = 
      minPercentTime.notMissing("minPercentTime").map(SystemVerification(_))
  
    override def edit: StateT[EitherInput, SystemVerification,Unit] = 
      for {
        minPct <- minPercentTime.validateIsNotNull("minPercentTime").liftState
        _      <- SystemVerification.minPercentTime := minPct
      } yield ()
  }

  object SystemVerificationInput {
    implicit val DecoderSystemVerificationInput: Decoder[SystemVerificationInput] =
      deriveConfiguredDecoder[SystemVerificationInput]

    implicit val EqSystemVerificationInput: Eq[SystemVerificationInput] =
      Eq.by { a => a.minPercentTime }
  }

  final case class LargeProgramInput(
    minPercentTime:      Input[IntPercent]           = Input.ignore,
    minPercentTotalTime: Input[IntPercent]           = Input.ignore,
    totalTime:           Input[NonNegDurationInput]  = Input.ignore
  ) extends EditorInput[LargeProgram] {
    override def create: lucuma.odb.api.model.ValidatedInput[LargeProgram] = 
      (minPercentTime.notMissing("minPercentTime"),
       minPercentTotalTime.notMissing("minPercentTotalTime"),
       totalTime.notMissing("totalTime").andThen(_.toNonNegDuration("totalTime"))
      ).mapN { case (mpt, mptt, tt) =>
        LargeProgram(mpt, mptt, tt)
      }
  
    override def edit: StateT[EitherInput, LargeProgram,Unit] = {
      val validArgs = 
        (minPercentTime.validateIsNotNull("minPercentTime"),
         minPercentTotalTime.validateIsNotNull("minPercentTotalTime"),
         totalTime.validateIsNotNull("totalTime").andThen(_.map(_.toNonNegDuration("totalTime")).sequence)
        ).tupled

      for {
        args <- validArgs.liftState
        (mpt, mptt, tt) = args
        _    <- LargeProgram.minPercentTime      := mpt
        _    <- LargeProgram.minPercentTotalTime := mptt
        _    <- LargeProgram.totalTime           := tt
      } yield ()
    }
  }

  object LargeProgramInput {
    implicit val DecoderLargeProgramInput: Decoder[LargeProgramInput] =
      deriveConfiguredDecoder[LargeProgramInput]

    implicit val EqLargeProgramInput: Eq[LargeProgramInput] =
      Eq.by { a => (a.minPercentTime, a.minPercentTotalTime, a.totalTime) }
  }

  final case class IntensiveInput(
    minPercentTime:      Input[IntPercent]           = Input.ignore,
    minPercentTotalTime: Input[IntPercent]           = Input.ignore,
    totalTime:           Input[NonNegDurationInput]  = Input.ignore
  ) extends EditorInput[Intensive] {
    override def create: lucuma.odb.api.model.ValidatedInput[Intensive] = 
      (minPercentTime.notMissing("minPercentTime"),
       minPercentTotalTime.notMissing("minPercentTotalTime"),
       totalTime.notMissing("totalTime").andThen(_.toNonNegDuration("totalTime"))
      ).mapN { case (mpt, mptt, tt) =>
        Intensive(mpt, mptt, tt)
      }
  
    override def edit: StateT[EitherInput, Intensive,Unit] = {
      val validArgs = 
        (minPercentTime.validateIsNotNull("minPercentTime"),
         minPercentTotalTime.validateIsNotNull("minPercentTotalTime"),
         totalTime.validateIsNotNull("totalTime").andThen(_.map(_.toNonNegDuration("totalTime")).sequence)
        ).tupled

      for {
        args <- validArgs.liftState
        (mpt, mptt, tt) = args
        _    <- Intensive.minPercentTime      := mpt
        _    <- Intensive.minPercentTotalTime := mptt
        _    <- Intensive.totalTime           := tt
      } yield ()
    }
  }

  object IntensiveInput {
    implicit val DecoderIntensiveInput: Decoder[IntensiveInput] =
      deriveConfiguredDecoder[IntensiveInput]

    implicit val EqIntensiveInput: Eq[IntensiveInput] =
      Eq.by { a => (a.minPercentTime, a.minPercentTotalTime, a.totalTime) }
  }
}

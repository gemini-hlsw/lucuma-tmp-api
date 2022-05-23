// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.data.StateT
import cats.syntax.eq._
import cats.syntax.functor._
import cats.syntax.validated._
import clue.data.Input
import clue.data.syntax._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.all.{PosBigDecimal, PosInt}
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.refined._
import lucuma.odb.api.model.DurationModel.NonNegDurationInput
import lucuma.odb.api.model.time.NonNegDuration
import lucuma.odb.api.model.syntax.validatedinput._
import monocle.Prism
import monocle.macros.GenPrism

sealed trait ExposureMode extends Product with Serializable

object ExposureMode {

  final case class SignalToNoise(value: PosBigDecimal)                extends ExposureMode
  final case class FixedExposure(count: PosInt, time: NonNegDuration) extends ExposureMode

  implicit val EqExposureMode: Eq[ExposureMode] =
    Eq.instance {
      case (SignalToNoise(a), SignalToNoise(b))           => a === b
      case (FixedExposure(ac, ad), FixedExposure(bc, bd)) => ac === bc && ad.equals(bd)
      case _                                              => false
    }

  final case class SignalToNoiseInput(
    value: PosBigDecimal
  ) extends EditorInput[SignalToNoise] {

    def toSignalToNoise: SignalToNoise =
      SignalToNoise(value)

    override def create: ValidatedInput[SignalToNoise] =
      toSignalToNoise.validNec[InputError]

    override def edit: StateT[EitherInput, SignalToNoise, Unit] =
      create.liftState[SignalToNoise].void

  }

  object SignalToNoiseInput {

    implicit val DecoderSignalToNoiseInput: Decoder[SignalToNoiseInput] =
      deriveDecoder[SignalToNoiseInput]

    implicit val EqSignalToNoiseInput: Eq[SignalToNoiseInput] =
      Eq.by(_.value)

  }

  final case class FixedExposureInput(
    count: PosInt,
    time:  NonNegDurationInput
  ) extends EditorInput[FixedExposure] {

    override def create: ValidatedInput[FixedExposure] =
      time
        .toNonNegDuration("time")
        .map(t => FixedExposure(count, t))

    override def edit: StateT[EitherInput, FixedExposure, Unit] =
      create.liftState[FixedExposure].void

  }

  object FixedExposureInput {

    implicit val DecoderFixedExposureInput: Decoder[FixedExposureInput] =
      deriveDecoder[FixedExposureInput]

    implicit val EqFixedExposureInput: Eq[FixedExposureInput] =
      Eq.by { a => (
        a.count,
        a.time
      )}
  }

  final case class ExposureModeInput(
    signalToNoise: Input[SignalToNoiseInput],
    fixedExposure: Input[FixedExposureInput]
  ) extends EditorInput[ExposureMode] {

    override def create: ValidatedInput[ExposureMode] =
      ValidatedInput.requireOne("exposureMode",
        signalToNoise.toOption.map(_.create),
        fixedExposure.toOption.map(_.create)
      )

    override def edit: StateT[EitherInput, ExposureMode, Unit] =
      create.liftState[ExposureMode].void

  }

  object ExposureModeInput {

    def signalToNoise(s: SignalToNoiseInput): ExposureModeInput =
      ExposureModeInput(s.assign, Input.ignore)

    def fixedExposure(f: FixedExposureInput): ExposureModeInput =
      ExposureModeInput(Input.ignore, f.assign)

    implicit val DecoderExposureModeInput: Decoder[ExposureModeInput] =
      deriveDecoder[ExposureModeInput]

    implicit val EqExposureModeInput: Eq[ExposureModeInput] =
      Eq.by { a => (
        a.signalToNoise,
        a.fixedExposure
      )}

  }

  val signalToNoise: Prism[ExposureMode, SignalToNoise] =
    GenPrism[ExposureMode, SignalToNoise]

  val fixedExposure: Prism[ExposureMode, FixedExposure] =
    GenPrism[ExposureMode, FixedExposure]

}

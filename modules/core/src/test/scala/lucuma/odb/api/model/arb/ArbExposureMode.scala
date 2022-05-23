// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import clue.data.Input
import eu.timepit.refined.types.all.{PosBigDecimal, PosInt}
import eu.timepit.refined.scalacheck.all._
import lucuma.core.math.arb.ArbRefined
import lucuma.odb.api.model.time.NonNegDuration
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary


sealed trait ArbExposureMode {

  import ExposureMode._

  import ArbDurationModel._
  import ArbNonNegDuration._
  import ArbInput._
  import ArbRefined._

  implicit val arbSignalToNoise: Arbitrary[SignalToNoise] =
    Arbitrary {
      arbitrary[PosBigDecimal].map(SignalToNoise)
    }

  implicit val cogSignalToNoise: Cogen[SignalToNoise] =
    Cogen[PosBigDecimal].contramap(_.value)

  implicit val arbSignalToNoiseInput: Arbitrary[SignalToNoiseInput] =
    Arbitrary {
      arbitrary[PosBigDecimal].map(SignalToNoiseInput.apply)
    }

  implicit val cogSignalToNoiseInput: Cogen[SignalToNoiseInput] =
    Cogen[PosBigDecimal].contramap(_.value)

  implicit val arbFixedExposure: Arbitrary[FixedExposure] =
    Arbitrary {
      for {
        c <- arbitrary[PosInt]
        t <- arbitrary[NonNegDuration]
      } yield FixedExposure(c, t)
    }

  implicit val cogFixedExposure: Cogen[FixedExposure] =
    Cogen[(
      PosInt,
      NonNegDuration
    )].contramap { in => (
      in.count,
      in.time
    )}

  implicit val arbFixedExposureInput: Arbitrary[FixedExposureInput] =
    Arbitrary {
      for {
        c <- arbitrary[PosInt]
        t <- arbitrary[DurationModel.NonNegDurationInput]
      } yield FixedExposureInput(c, t)
    }

  implicit val cogFixedExposureInput: Cogen[FixedExposureInput] =
    Cogen[(PosInt, DurationModel.NonNegDurationInput)].contramap { a => (
      a.count,
      a.time
    )}

  implicit val arbExposureMode: Arbitrary[ExposureMode] =
    Arbitrary {
      Gen.oneOf(arbitrary[FixedExposure], arbitrary[FixedExposure])
    }

  implicit val cogExposureMode: Cogen[ExposureMode] =
    Cogen[(
      Option[SignalToNoise],
      Option[FixedExposure]
    )].contramap { in => (
      ExposureMode.signalToNoise.getOption(in),
      ExposureMode.fixedExposure.getOption(in)
    )}

  implicit val arbExposureModeInput: Arbitrary[ExposureModeInput] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[SignalToNoiseInput].map(ExposureModeInput.signalToNoise),
        arbitrary[FixedExposureInput].map(ExposureModeInput.fixedExposure)
      )
    }

  implicit val cogExposureModeInput: Cogen[ExposureModeInput] =
    Cogen[(
      Input[SignalToNoiseInput],
      Input[FixedExposureInput]
    )].contramap { in => (
      in.signalToNoise,
      in.fixedExposure
    )}
}

object ArbExposureMode extends ArbExposureMode

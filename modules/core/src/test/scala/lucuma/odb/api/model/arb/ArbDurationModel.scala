// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.core.math.arb.ArbRefined
import DurationModel.NonNegDurationInput
import eu.timepit.refined.types.all.{NonNegBigDecimal, NonNegLong}
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck._


trait ArbDurationModel {

  import ArbEnumerated._
  import ArbRefined._

  private[this] val c_µs = 1000L
  private[this] val c_ms = c_µs * 1000L
  private[this] val c_s  = c_ms * 1000L
  private[this] val c_m  = c_s  * 60L
  private[this] val c_h  = c_m  * 60L
  private[this] val c_d  = c_h  * 24L

  private def genBigDecimal(c: Long): Gen[NonNegBigDecimal] = {
    val max = Long.MaxValue/c-1
    for {
      h <- Gen.chooseNum(0, max)
      f <- Gen.chooseNum(0, 499)
    } yield NonNegBigDecimal.unsafeFrom(BigDecimal(h) + BigDecimal(f)/1000)
  }

  private[this] val microseconds: Gen[NonNegLong]       =
    Gen.chooseNum[Long](0L, Long.MaxValue/1000-1).map(NonNegLong.unsafeFrom)

  private[this] val milliseconds: Gen[NonNegBigDecimal] = genBigDecimal(c_ms)
  private[this] val seconds: Gen[NonNegBigDecimal]      = genBigDecimal(c_s)
  private[this] val minutes: Gen[NonNegBigDecimal]      = genBigDecimal(c_m)
  private[this] val hours: Gen[NonNegBigDecimal]        = genBigDecimal(c_h)
  private[this] val days: Gen[NonNegBigDecimal]         = genBigDecimal(c_d)

  implicit val arbDurationModelInput: Arbitrary[DurationModel.NonNegDurationInput] =
    Arbitrary {
      Gen.oneOf(
        microseconds.map(NonNegDurationInput.fromMicroseconds),
        milliseconds.map(NonNegDurationInput.fromMilliseconds),
        seconds.map(NonNegDurationInput.fromSeconds),
        minutes.map(NonNegDurationInput.fromMinutes),
        hours.map(NonNegDurationInput.fromHours),
        days.map(NonNegDurationInput.fromDays),
      )
    }

  implicit val cogDurationModelInput: Cogen[DurationModel.NonNegDurationInput] =
    Cogen[(
      Option[NonNegLong],        // µs
      Option[NonNegBigDecimal],  // ms
      Option[NonNegBigDecimal],  // s
      Option[NonNegBigDecimal],  // m
      Option[NonNegBigDecimal],  // h
      Option[NonNegBigDecimal]   // d
    )].contramap { in =>
      (
        in.microseconds,
        in.milliseconds,
        in.seconds,
        in.minutes,
        in.hours,
        in.days
      )
    }

}

object ArbDurationModel extends ArbDurationModel

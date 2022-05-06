// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import DurationModel.{Input, Units}
import NumericUnits.{LongInput, DecimalInput}

import lucuma.core.util.arb.ArbEnumerated

import org.scalacheck._


trait ArbDurationModel {

  import ArbEnumerated._
  import GenNumericUnitsInput._

  private[this] val c_µs = 1L
  private[this] val c_ms = c_µs * 1000L
  private[this] val c_s  = c_ms * 1000L
  private[this] val c_m  = c_s  * 60L
  private[this] val c_h  = c_m  * 60L
  private[this] val c_d  = c_h  * 24L

  private def genBigDecimal(c: Long): Gen[BigDecimal] = {
    val max = Long.MaxValue/c
    for {
      h <- Gen.chooseNum(-max, max)
      f <- Gen.chooseNum(0, 499)
    } yield BigDecimal(h) + BigDecimal(f)/1000
  }

  private[this] val microseconds: Gen[Long]       =
    Gen.chooseNum[Long](-Long.MaxValue, Long.MaxValue)

  private[this] val milliseconds: Gen[BigDecimal] = genBigDecimal(c_ms)
  private[this] val seconds: Gen[BigDecimal]      = genBigDecimal(c_s)
  private[this] val minutes: Gen[BigDecimal]      = genBigDecimal(c_m)
  private[this] val hours: Gen[BigDecimal]        = genBigDecimal(c_h)
  private[this] val days: Gen[BigDecimal]         = genBigDecimal(c_d)

  val genDurationModelInputFromLong: Gen[Input] =
    Gen.oneOf(
      genLongInput(microseconds, Units.microseconds),
      genLongInput(milliseconds, Units.milliseconds),
      genLongInput(seconds,      Units.seconds),
      genLongInput(minutes,      Units.minutes),
      genLongInput(hours,        Units.hours),
      genLongInput(days,         Units.days)
    ).map(Input.fromLong)

  val genDurationModelInputFromDecimal: Gen[Input] =
    Gen.oneOf(
      genLongDecimalInput(microseconds, Units.microseconds),
      genDecimalInput(milliseconds, Units.milliseconds),
      genDecimalInput(seconds,      Units.seconds),
      genDecimalInput(minutes,      Units.minutes),
      genDecimalInput(hours,        Units.hours),
      genDecimalInput(days,         Units.days)
    ).map(Input.fromDecimal)


  implicit val arbDurationModelInput: Arbitrary[DurationModel.Input] =
    Arbitrary {
      Gen.oneOf(
        microseconds.map(Input.fromMicroseconds),
        milliseconds.map(Input.fromMilliseconds),
        seconds.map(Input.fromSeconds),
        minutes.map(Input.fromMinutes),
        hours.map(Input.fromHours),
        days.map(Input.fromDays),
        genDurationModelInputFromLong,
        genDurationModelInputFromDecimal
      )
    }

  implicit val cogDurationModelInput: Cogen[DurationModel.Input] =
    Cogen[(
      Option[Long],        // µs
      Option[BigDecimal],  // ms
      Option[BigDecimal],  // s
      Option[BigDecimal],  // m
      Option[BigDecimal],  // h
      Option[BigDecimal],  // d
      Option[LongInput[Units]],
      Option[DecimalInput[Units]]
    )].contramap { in =>
      (
        in.microseconds,
        in.milliseconds,
        in.seconds,
        in.minutes,
        in.hours,
        in.days,
        in.fromLong,
        in.fromDecimal
      )
    }

}

object ArbDurationModel extends ArbDurationModel

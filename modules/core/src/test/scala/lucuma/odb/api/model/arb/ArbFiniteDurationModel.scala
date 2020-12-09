// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import FiniteDurationModel.{Input, Units}
import NumericUnits.{LongInput, DecimalInput}

import lucuma.core.util.arb.ArbEnumerated

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary


trait ArbFiniteDurationModel {

  import ArbEnumerated._
  import GenNumericUnitsInput._

  private[this] val nanoseconds: Gen[Long] =
    arbitrary[Long]

  private[this] val microseconds: Gen[BigDecimal] =
    nanoseconds.map(n => BigDecimal(n)/1000)

  private[this] val milliseconds: Gen[BigDecimal] =
    microseconds.map(_/1000)

  private[this] val seconds: Gen[BigDecimal] =
    milliseconds.map(_/1000)

  private[this] val minutes: Gen[BigDecimal] =
    seconds.map(_/60)

  private[this] val hours: Gen[BigDecimal] =
    minutes.map(_/60)

  private[this] val days: Gen[BigDecimal] =
    hours.map(_/24)

  val genFiniteDurationModelInputFromLong: Gen[Input] =
    Gen.oneOf(
      genLongInput(nanoseconds,  Units.nanoseconds),
      genLongInput(microseconds, Units.microseconds),
      genLongInput(milliseconds, Units.milliseconds),
      genLongInput(seconds,      Units.seconds),
      genLongInput(minutes,      Units.minutes),
      genLongInput(hours,        Units.hours),
      genLongInput(days,         Units.days)
    ).map(Input.fromLong)

  val genFiniteDurationModelInputFromDecimal: Gen[Input] =
    Gen.oneOf(
      genDecimalInput(nanoseconds,  Units.nanoseconds),
      genDecimalInput(microseconds, Units.microseconds),
      genDecimalInput(milliseconds, Units.milliseconds),
      genDecimalInput(seconds,      Units.seconds),
      genDecimalInput(minutes,      Units.minutes),
      genDecimalInput(hours,        Units.hours),
      genDecimalInput(days,         Units.days)
    ).map(Input.fromDecimal)


  implicit val arbFiniteDurationModelInput: Arbitrary[FiniteDurationModel.Input] =
    Arbitrary {
      Gen.oneOf(
        nanoseconds.map(Input.fromNanoseconds),
        microseconds.map(Input.fromMicroseconds),
        milliseconds.map(Input.fromMilliseconds),
        seconds.map(Input.fromSeconds),
        minutes.map(Input.fromMinutes),
        hours.map(Input.fromHours),
        days.map(Input.fromDays),
        genFiniteDurationModelInputFromLong,
        genFiniteDurationModelInputFromDecimal
      )
    }

  implicit val cogFiniteDurationModelInput: Cogen[FiniteDurationModel.Input] =
    Cogen[(
      Option[Long],        // ns
      Option[BigDecimal],  // µs
      Option[BigDecimal],  // ms
      Option[BigDecimal],  // s
      Option[BigDecimal],  // m
      Option[BigDecimal],  // h
      Option[BigDecimal],  // d
      Option[LongInput[Units]],
      Option[DecimalInput[Units]]
    )].contramap { in =>
      (
        in.nanoseconds,
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

object ArbFiniteDurationModel extends ArbFiniteDurationModel

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.syntax.time._
import lucuma.core.util.{Display, Enumerated}
import cats.Eq
import cats.syntax.option._
import io.circe.Decoder
import io.circe.generic.semiauto._
import monocle.Prism

import java.time.Duration
import java.time.temporal.ChronoUnit
import scala.math.BigDecimal.RoundingMode
import scala.util.Try

object DurationModel {

  sealed abstract class Units(
    val timeUnit: ChronoUnit
  ) extends Product with Serializable {

    val long: Prism[Long, Duration] =
      Prism[Long, Duration](l => Try(Duration.of(l, timeUnit)).toOption)(_.get(timeUnit))

    def readLong(l: Long): ValidatedInput[Duration] =
      long.getOption(l).toValidNec(
        InputError.fromMessage(
          s"Could not read $l ${timeUnit.toString} as a time amount"
        )
      )

    val decimal: Prism[BigDecimal, Duration] =
      Prism[BigDecimal, Duration](bd => Try(Duration.of(bd.setScale(0, RoundingMode.HALF_UP).longValue, timeUnit)).toOption)(fd => BigDecimal(fd.get(timeUnit)))

    def readDecimal(b: BigDecimal): ValidatedInput[Duration] =
      decimal.getOption(b).toValidNec(
        InputError.fromMessage(
          s"Could not read $b ${timeUnit.toString} as a time amount"
        )
      )
  }

  object Units {
    case object Microseconds extends Units(ChronoUnit.MICROS)
    case object Milliseconds extends Units(ChronoUnit.MILLIS)
    case object Seconds      extends Units(ChronoUnit.SECONDS)
    case object Minutes      extends Units(ChronoUnit.MINUTES)
    case object Hours        extends Units(ChronoUnit.HOURS)
    case object Days         extends Units(ChronoUnit.DAYS)

    val microseconds: Units = Microseconds
    val milliseconds: Units = Milliseconds
    val seconds: Units      = Seconds
    val minutes: Units      = Minutes
    val hours: Units        = Hours
    val days: Units         = Days

    implicit val EnumeratedUnits: Enumerated[Units] =
      Enumerated.of(
        Microseconds,
        Milliseconds,
        Seconds,
        Minutes,
        Hours,
        Days
      )

    implicit val DisplayUnits: Display[Units] =
      Display.byShortName(_.timeUnit.name)
  }

  implicit val NumericUnitsDuration: NumericUnits[Duration, Units] =
    NumericUnits.fromRead(_.readLong(_), _.readDecimal(_))

  final case class Input(
    microseconds: Option[Long],
    milliseconds: Option[BigDecimal],
    seconds:      Option[BigDecimal],
    minutes:      Option[BigDecimal],
    hours:        Option[BigDecimal],
    days:         Option[BigDecimal],
    fromLong:     Option[NumericUnits.LongInput[Units]],
    fromDecimal:  Option[NumericUnits.DecimalInput[Units]]
  ) {

    import Units._

    def toDuration(n: String): ValidatedInput[Duration] =
      ValidatedInput.requireOne(n,
        microseconds.map(Microseconds.readLong),
        milliseconds.map(Milliseconds.readDecimal),
        seconds     .map(Seconds.readDecimal),
        minutes     .map(Minutes.readDecimal),
        hours       .map(Hours.readDecimal),
        days        .map(Days.readDecimal),
        fromLong    .map(_.read),
        fromDecimal .map(_.read)
      )

  }

  object Input {

    val Empty: Input =
      Input(None, None, None, None, None, None, None, None)

    def apply(fd: Duration): Input =
      fromMicroseconds(fd.toMicros)

    def fromMicroseconds(value: Long): Input =
      Empty.copy(microseconds = Some(value))

    def fromMilliseconds(value: BigDecimal): Input =
      Empty.copy(milliseconds = Some(value))

    def fromSeconds(value: BigDecimal): Input =
      Empty.copy(seconds = Some(value))

    def fromMinutes(value: BigDecimal): Input =
      Empty.copy(minutes = Some(value))

    def fromHours(value: BigDecimal): Input =
      Empty.copy(hours = Some(value))

    def fromDays(value: BigDecimal): Input =
      Empty.copy(days = Some(value))

    def fromLong(value: NumericUnits.LongInput[Units]): Input =
      Empty.copy(fromLong = Some(value))

    def fromDecimal(value: NumericUnits.DecimalInput[Units]): Input =
      Empty.copy(fromDecimal = Some(value))

    implicit val DecoderInput: Decoder[Input] =
      deriveDecoder[Input]

    implicit val EqInput: Eq[Input] =
      Eq.by(in => (
        in.microseconds,
        in.milliseconds,
        in.seconds,
        in.minutes,
        in.hours,
        in.days,
        in.fromLong,
        in.fromDecimal
      ))
  }

}

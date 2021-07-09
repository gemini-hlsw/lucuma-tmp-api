// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.util.{Display, Enumerated}
import cats.Eq
import cats.syntax.option._
import io.circe.Decoder
import io.circe.generic.semiauto._
import monocle.Prism

import scala.concurrent.duration._
import scala.math.BigDecimal.RoundingMode
import scala.util.Try

object FiniteDurationModel {

  sealed abstract class Units(
    val timeUnit: TimeUnit
  ) extends Product with Serializable {

    val long: Prism[Long, FiniteDuration] =
      Prism[Long, FiniteDuration](l => Try(FiniteDuration(l, timeUnit)).toOption)(_.length)

    def readLong(l: Long): ValidatedInput[FiniteDuration] =
      long.getOption(l).toValidNec(
        InputError.fromMessage(
          s"Could not read $l ${timeUnit.toString} as a time amount"
        )
      )

    val decimal: Prism[BigDecimal, FiniteDuration] =
      Prism[BigDecimal, FiniteDuration](bd => Try(FiniteDuration(bd.setScale(0, RoundingMode.HALF_UP).longValue, timeUnit)).toOption)(fd => BigDecimal(fd.length))

    def readDecimal(b: BigDecimal): ValidatedInput[FiniteDuration] =
      decimal.getOption(b).toValidNec(
        InputError.fromMessage(
          s"Could not read $b ${timeUnit.toString} as a time amount"
        )
      )
  }

  object Units {
    case object Nanoseconds  extends Units(NANOSECONDS)
    case object Microseconds extends Units(MICROSECONDS)
    case object Milliseconds extends Units(MILLISECONDS)
    case object Seconds      extends Units(SECONDS)
    case object Minutes      extends Units(MINUTES)
    case object Hours        extends Units(HOURS)
    case object Days         extends Units(DAYS)

    val nanoseconds: Units  = Nanoseconds
    val microseconds: Units = Microseconds
    val milliseconds: Units = Milliseconds
    val seconds: Units      = Seconds
    val minutes: Units      = Minutes
    val hours: Units        = Hours
    val days: Units         = Days

    implicit val EnumeratedUnits: Enumerated[Units] =
      Enumerated.of(
        Nanoseconds,
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

  implicit val NumericUnitsFiniteDuration: NumericUnits[FiniteDuration, Units] =
    NumericUnits.fromRead(_.readLong(_), _.readDecimal(_))

  final case class Input(
    nanoseconds:  Option[Long],
    microseconds: Option[BigDecimal],
    milliseconds: Option[BigDecimal],
    seconds:      Option[BigDecimal],
    minutes:      Option[BigDecimal],
    hours:        Option[BigDecimal],
    days:         Option[BigDecimal],
    fromLong:     Option[NumericUnits.LongInput[Units]],
    fromDecimal:  Option[NumericUnits.DecimalInput[Units]]
  ) {

    import Units._

    def toFiniteDuration(n: String): ValidatedInput[FiniteDuration] =
      ValidatedInput.requireOne(n,
        nanoseconds .map(Nanoseconds.readLong),
        microseconds.map(Microseconds.readDecimal),
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
      Input(None, None, None, None, None, None, None, None, None)

    def apply(fd: FiniteDuration): Input =
      fromNanoseconds(fd.toNanos)

    def fromNanoseconds(value: Long): Input =
      Empty.copy(nanoseconds = Some(value))

    def fromMicroseconds(value: BigDecimal): Input =
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
        in.nanoseconds,
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

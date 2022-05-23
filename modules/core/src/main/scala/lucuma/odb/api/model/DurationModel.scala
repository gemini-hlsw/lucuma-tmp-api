// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.syntax.option._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.all.{NonNegBigDecimal, NonNegLong}
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.refined._
import lucuma.core.syntax.time._
import lucuma.core.util.{Display, Enumerated}
import lucuma.odb.api.model.time.NonNegDuration

import java.time.Duration
import java.time.temporal.ChronoUnit

import scala.math.BigDecimal.RoundingMode
import scala.util.Try

object DurationModel {

  sealed abstract class Units(
    val timeUnit: ChronoUnit
  ) extends Product with Serializable {

    def readLong(nnl: NonNegLong): ValidatedInput[NonNegDuration] =
      Try(Duration.of(nnl.value, timeUnit))
        .toOption
        .flatMap(NonNegDuration.from(_).toOption)
        .toValidNec(
          InputError.fromMessage(
            s"Could not read $nnl ${timeUnit.toString} as a time amount"
          )
        )

    def readDecimal(nnbd: NonNegBigDecimal): ValidatedInput[NonNegDuration] =
      Try(Duration.of(nnbd.value.setScale(0, RoundingMode.HALF_UP).longValue, timeUnit))
        .toOption
        .flatMap(NonNegDuration.from(_).toOption)
        .toValidNec(
          InputError.fromMessage(
            s"Could not read ${nnbd.value} ${timeUnit.toString} as a time amount"
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

  final case class NonNegDurationInput(
    microseconds: Option[NonNegLong],
    milliseconds: Option[NonNegBigDecimal],
    seconds:      Option[NonNegBigDecimal],
    minutes:      Option[NonNegBigDecimal],
    hours:        Option[NonNegBigDecimal],
    days:         Option[NonNegBigDecimal]
  ) {

    import Units._

    def toNonNegDuration(n: String): ValidatedInput[NonNegDuration] =
      ValidatedInput.requireOne(n,
        microseconds.map(Microseconds.readLong),
        milliseconds.map(Milliseconds.readDecimal),
        seconds     .map(Seconds.readDecimal),
        minutes     .map(Minutes.readDecimal),
        hours       .map(Hours.readDecimal),
        days        .map(Days.readDecimal)
      )

  }

  object NonNegDurationInput {

    val Empty: NonNegDurationInput =
      NonNegDurationInput(None, None, None, None, None, None)

    def unsafeFromDuration(fd: Duration): NonNegDurationInput =
      fromMicroseconds(NonNegLong.unsafeFrom(fd.toMicros))

    def fromMicroseconds(value: NonNegLong): NonNegDurationInput =
      Empty.copy(microseconds = Some(value))

    def fromMilliseconds(value: NonNegBigDecimal): NonNegDurationInput =
      Empty.copy(milliseconds = Some(value))

    def fromSeconds(value: NonNegBigDecimal): NonNegDurationInput =
      Empty.copy(seconds = Some(value))

    def fromMinutes(value: NonNegBigDecimal): NonNegDurationInput =
      Empty.copy(minutes = Some(value))

    def fromHours(value: NonNegBigDecimal): NonNegDurationInput =
      Empty.copy(hours = Some(value))

    def fromDays(value: NonNegBigDecimal): NonNegDurationInput =
      Empty.copy(days = Some(value))

    implicit val DecoderNonNegDurationInput: Decoder[NonNegDurationInput] =
      deriveDecoder[NonNegDurationInput]

    implicit val EqNonNegDurationInput: Eq[NonNegDurationInput] =
      Eq.by(in => (
        in.microseconds,
        in.milliseconds,
        in.seconds,
        in.minutes,
        in.hours,
        in.days
      ))
  }

}

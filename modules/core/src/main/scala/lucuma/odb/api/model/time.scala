// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Monoid
import eu.timepit.refined.api.{Refined, RefinedTypeOps, Validate}
import eu.timepit.refined.api.Validate.Plain
import eu.timepit.refined.boolean.{And, Not}
import eu.timepit.refined.numeric.{Greater, GreaterEqual, Interval, Less, NonNegative}
import io.circe.Decoder
import lucuma.core.syntax.time._
import shapeless.Nat._0

import java.time.temporal.Temporal
import java.time.{Duration, LocalDate, Month}

import scala.util.Try

object time {

  implicit val nonNegDurationValidate: Plain[Duration, GreaterEqual[_0]] =
    Validate.fromPredicate(
      (d: Duration) => d.toNanos >= 0L,
      (d: Duration) => s"$d is not negative",
      Not(Less(shapeless.nat._0))
    )

  type NonNegDuration = Duration Refined NonNegative

  object NonNegDuration extends RefinedTypeOps[NonNegDuration, Duration] {

    val zero: NonNegDuration =
      unsafeFrom(Duration.ofNanos(0L))

    def between(startInclusive: Temporal, endExclusive: Temporal): NonNegDuration =
      NonNegDuration.unapply(Duration.between(startInclusive, endExclusive))
        .getOrElse(zero)

  }

  // Has to be a def or else there are initialization issues.
  implicit def nonNegDurationMonoid: Monoid[NonNegDuration] =
    Monoid.instance(
      NonNegDuration.zero,
      (a, b) => NonNegDuration.unsafeFrom(a.value + b.value)
    )

  implicit val fourDigitYearLocalDateValidate: Plain[LocalDate, Interval.Closed[0, 9999]] =
    Validate.fromPredicate(
      (ld: LocalDate) => 0L <= ld.getYear && ld.getYear <= 9999,
      (ld: LocalDate) => s"$ld year in range [0, 9999]",
      And(Not(Less(0)), Not(Greater(9999)))
    )

  /**
   * A LocalDate with year between 0000 and 9999 (inclusive).
   */
  type FourDigitYearLocalDate = LocalDate Refined Interval.Closed[0, 9999]

  object FourDigitYearLocalDate extends RefinedTypeOps[FourDigitYearLocalDate, LocalDate] {

    val MinDate: FourDigitYearLocalDate =
      unsafeFrom(LocalDate.of(0, Month.JANUARY, 1))

    val MaxDate: FourDigitYearLocalDate =
      unsafeFrom(LocalDate.of(9999, Month.DECEMBER, 31))

    def fromYMD(year: Int, month: Int, day: Int): Option[FourDigitYearLocalDate] =
      Try(LocalDate.of(year, month, day)).map(unsafeFrom).toOption

  }

  implicit val DecoderFourDigitYearLocalDate: Decoder[FourDigitYearLocalDate] =
    Decoder[LocalDate].emap(FourDigitYearLocalDate.from)

}

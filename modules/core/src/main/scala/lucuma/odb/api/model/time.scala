// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Monoid
import eu.timepit.refined.api.{Refined, RefinedTypeOps, Validate}
import eu.timepit.refined.api.Validate.Plain
import eu.timepit.refined.boolean.{And, Not}
import eu.timepit.refined.numeric.{Greater, GreaterEqual, Interval, Less, NonNegative}
import io.circe.Decoder
import shapeless.Nat._0

import java.time.{LocalDate, Month}
import java.util.concurrent.TimeUnit

import scala.concurrent.duration._
import scala.util.Try

object time {

  implicit val nonNegativeFiniteDurationValidate: Plain[FiniteDuration, GreaterEqual[_0]] =
    Validate.fromPredicate(
      (d: FiniteDuration) => d.length >= 0L,
      (d: FiniteDuration) => s"$d is not negative",
      Not(Less(shapeless.nat._0))
    )

  type NonNegativeFiniteDuration = FiniteDuration Refined NonNegative

  object NonNegativeFiniteDuration extends RefinedTypeOps[NonNegativeFiniteDuration, FiniteDuration] {

    def zero(unit: TimeUnit): NonNegativeFiniteDuration =
      unsafeFrom(FiniteDuration(0L, unit))

  }

  // Has to be a def or else there are initialization issues.
  implicit def nonNegFiniteDurationMonoid: Monoid[NonNegativeFiniteDuration] =
    Monoid.instance(
      NonNegativeFiniteDuration.zero(TimeUnit.DAYS),
      (a, b) => NonNegativeFiniteDuration.unsafeFrom(a.value + b.value)
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

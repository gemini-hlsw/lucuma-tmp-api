// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Semigroup
import eu.timepit.refined.api.{Refined, RefinedTypeOps, Validate}
import eu.timepit.refined.api.Validate.Plain
import eu.timepit.refined.boolean.Not
import eu.timepit.refined.numeric.{GreaterEqual, Less, NonNegative}
import lucuma.odb.api.model.duration.NonNegativeFiniteDuration.unsafeFrom
import shapeless.Nat._0

import scala.concurrent.duration.{FiniteDuration, TimeUnit}

object duration {

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

  // FiniteDuration seems very monoidy but the presence of the TimeUnit spoils it.
  implicit val nonNegFiniteDurationSemigroup: Semigroup[NonNegativeFiniteDuration] =
    Semigroup.instance[NonNegativeFiniteDuration] { (a, b) =>
      unsafeFrom(a.value + b.value)
    }

}
